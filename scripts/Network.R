library(igraph)
library(tibble)

# =============================================================
# ATTRACTIVENESS FUNCTION
# Ideological proximity x competence sigmoid
# =============================================================

compute_attractiveness <- function(pref_i, pref_j, pow_i, pow_j, resp) {
  (1 - abs(pref_i - pref_j)) / (1 + exp(-resp * (pow_j - pow_i)))
}

# =============================================================
# VECTORIZED ROOT SEARCH (helper for power + vote propagation)
#
# Idea: delegate_of[i] = j means i -> j (0 = votes directly / root).
# Instead of traversing each agent's chain individually (O(n * L)),
# in each iteration ALL agents jump one step forward simultaneously:
#
#   Round 1: pointer[i] = delegate_of[i]          (1 step)
#   Round 2: pointer[i] = delegate_of[pointer[i]] (2 steps)
#   ...
#
# After at most L iterations (L = length of longest chain) all
# pointers have reached their root or are stuck in a cycle.
# Cycles are identified by pointers that have not reached 0
# after n iterations.
#
# Cost: O(L * n) vector operations instead of O(n * L) R loops.
# In practice: L << n, and vector ops are ~100x faster than
# R loops -> substantial speedup for large n.
# =============================================================

find_roots_vectorized <- function(n, delegate_of) {
  # delegate_of: integer vector of length n
  # delegate_of[i] = j: i delegates to j; 0: i is a root
  
  pointer  <- seq_len(n)        # each agent initially points to itself
  in_cycle <- rep(FALSE, n)
  
  for (iter in seq_len(n)) {    # at most n steps to reach any root
    next_ptr <- delegate_of[pointer]  # jump one step forward
    moving   <- next_ptr != 0L        # not yet at root
    
    if (!any(moving)) break           # all agents have reached their root
    
    # Cycle detection: pointer has looped back to the starting agent
    looping          <- moving & next_ptr == seq_len(n)
    in_cycle[looping] <- TRUE
    moving[looping]   <- FALSE
    
    pointer[moving] <- next_ptr[moving]
  }
  
  # Agents still not at 0 after n steps -> cycle
  still_moving        <- delegate_of[pointer] != 0L & !in_cycle
  in_cycle[still_moving] <- TRUE
  
  # Root = pointer if delegate_of[pointer] == 0, else NA (cycle)
  roots <- ifelse(in_cycle, NA_integer_, pointer)
  roots
}

# =============================================================
# POWER COMPUTATION (vectorized)
# Every agent starts with power = 1 (their own vote).
# Agents who receive delegations accumulate power transitively:
#   power[root] += number of agents in their chain
# Cycles: agents in a cycle keep power = 1 (Christoff & Grossi 2017)
# =============================================================

compute_power <- function(n, edge_from, edge_to) {
  power <- rep(1L, n)
  if (!length(edge_from)) return(power)
  
  delegate_of <- integer(n)
  delegate_of[edge_from] <- edge_to
  
  roots <- find_roots_vectorized(n, delegate_of)
  
  # For each delegating agent: add 1 to their root's power
  valid <- edge_from[!is.na(roots[edge_from])]
  if (length(valid)) {
    root_counts <- tabulate(roots[valid], nbins = n)
    power <- power + root_counts
  }
  power
}

# =============================================================
# VOTE PROPAGATION (vectorized)
# Direct voters: keep their own preference as their vote
# Delegating agents: adopt the preference of their chain's root
# Cycle agents: vote is lost (NA)
# =============================================================

propagate_votes <- function(preference, n, edge_from, edge_to) {
  votes <- preference
  if (!length(edge_from)) return(votes)
  
  delegate_of <- integer(n)
  delegate_of[edge_from] <- edge_to
  
  roots <- find_roots_vectorized(n, delegate_of)
  
  # Delegating agents inherit the preference of their root
  for (i in edge_from) {
    r        <- roots[i]
    votes[i] <- if (is.na(r)) NA_real_ else preference[r]
  }
  votes
}

# =============================================================
# AGENT SETUP
# =============================================================

setup_agents <- function(n_per_community, n_communities,
                         n_experts_per_community, seed = 1) {
  set.seed(seed)
  n_lay <- n_per_community * n_communities
  n_exp <- n_experts_per_community * n_communities
  n_all <- n_lay + n_exp
  
  agents <- tibble(
    id         = 1:n_all,
    type       = c(rep("lay", n_lay), rep("expert", n_exp)),
    preference = runif(n_all),          # fixed ideological position on [0,1]
    community = c(
                (0:(n_lay - 1)) %% n_communities,
                if (n_exp > 0) (0:(n_exp - 1)) %% n_communities else integer(0)
                ),
    power      = 1L,                    # initialised to 1 (own vote)
    my_vote    = NA_real_,
    delegated  = FALSE
  )
  list(agents = agents, n_lay = n_lay, n_exp = n_exp, n_all = n_all)
}

# =============================================================
# FRIENDSHIP NETWORK (Watts-Strogatz Small-World)
#
# Construction:
#   1. Regular ring per community: each lay agent connected to
#      their k = node_degree/2 nearest neighbours on each side
#   2. Watts-Strogatz rewiring: each edge is redirected to a
#      random lay agent with probability p_rewire
#      p_rewire = 0  : pure ring (high clustering, long paths)
#      p_rewire ~ 0.05: small-world (high clustering, short paths)
#      p_rewire = 1  : random graph
#
# Experts:
#   Each expert is connected to ceil(expert_connectedness *
#   n_per_community) random lay agents in their community.
#   Direction: lay -> expert only.
#   Experts never delegate, so no expert -> lay edge is needed.
# =============================================================

setup_friendship_network <- function(agents, n_communities, node_degree,
                                     p_rewire = 0.05, seed = 1) {
  stopifnot(node_degree %% 2 == 0)
  set.seed(seed)
  
  lay_ids <- which(agents$type == "lay")
  k       <- node_degree %/% 2L
  
  # Build regular ring per community
  edge_list <- do.call(rbind, lapply(0:(n_communities - 1), function(com) {
    nc  <- sort(lay_ids[agents$community[lay_ids] == com])
    n_c <- length(nc)
    if (n_c < 2) return(NULL)
    from <- rep(nc, each = k)
    step <- rep(seq_len(k), times = n_c)
    to   <- nc[((match(from, nc) - 1L + step) %% n_c) + 1L]
    cbind(from, to)
  }))
  
  # Watts-Strogatz rewiring (within lay nodes only)
  if (p_rewire > 0) {
    for (i in seq_len(nrow(edge_list))) {
      if (runif(1) < p_rewire) {
        u        <- edge_list[i, 1]
        existing <- c(edge_list[edge_list[, 1] == u, 2],
                      edge_list[edge_list[, 2] == u, 1], u)
        cands    <- lay_ids[!lay_ids %in% existing]
        if (length(cands)) edge_list[i, 2] <- sample(cands, 1)
      }
    }
  }
  
  # Lay-to-lay edges are bidirectional (either can delegate to the other)
  edges_dir <- unique(rbind(edge_list, edge_list[, c(2, 1)]))
  gF <- graph_from_edgelist(edges_dir, directed = TRUE)
  gF <- add_vertices(gF, max(0, nrow(agents) - vcount(gF)))
  gF
}

connect_experts <- function(gF, agents, n_per_community,
                            expert_connectedness, seed = 1) {
  set.seed(seed)
  lay_ids <- which(agents$type == "lay")
  exp_ids <- which(agents$type == "expert")
  
  # if no experts exist, return the friendship graph unchanged
  if (length(exp_ids) == 0) return(gF)
  
  k <- ceiling(expert_connectedness * n_per_community)
  
  new_edges <- do.call(rbind, lapply(exp_ids, function(e) {
    lay_c  <- lay_ids[agents$community[lay_ids] == agents$community[e]]
    chosen <- sample(lay_c, min(k, length(lay_c)))
    cbind(chosen, e)
  }))
  
  add_edges(gF, as.vector(t(new_edges)))
}

# =============================================================
# MAIN SIMULATION
#
# Each of the T rounds:
#
#   LAY AGENTS:
#     Observe the power and preference of all neighbours from t-1.
#     Choose probabilistically:
#       (a) Vote directly (target = self)
#           Weight: sigmoid(own power) — the more trust I already
#           enjoy, the more likely I vote directly
#       (b) Delegate to neighbour j
#           Weight: attractiveness(i,j) = proximity x competence
#     This creates endogenous persistence: powerful delegates remain
#     attractive -> stable chains without artificial inertia
#
#   EXPERTS:
#     Always vote directly with their own preference.
#     Do not participate in delegation (never appear as edge_from).
#     Can be the target of lay delegations.
#
#   AFTER EACH ROUND:
#     Power is computed transitively (the root of a chain collects
#     all votes of agents in that chain).
#     Votes propagate along chains to the root.
# =============================================================

simulate_liquid_democracy <- function(
    seed                    = 123,
    n_per_community         = 50,
    n_communities           = 5,
    node_degree             = 6,
    n_experts_per_community = 2,
    expert_connectedness    = 0.2,
    p_rewire                = 0.05,
    responsiveness          = 1,
    T                       = 200
) {
  st     <- setup_agents(n_per_community, n_communities,
                         n_experts_per_community, seed)
  agents <- st$agents
  n_all  <- st$n_all
  
  gF <- setup_friendship_network(agents, n_communities, node_degree,
                                 p_rewire, seed)
  gF <- connect_experts(gF, agents, n_per_community,
                        expert_connectedness, seed)
  
  # adj[[i]]: neighbours of i in the friendship network (outgoing edges)
  # For lay agents this includes other lay agents + reachable experts
  adj     <- lapply(seq_len(n_all),
                    \(v) as.integer(neighbors(gF, v, mode = "out")))
  lay_ids <- which(agents$type == "lay")
  pref    <- agents$preference   # preferences are fixed throughout
  
  history           <- numeric(T)
  delegation_graphs <- vector("list", T)
  
  for (t in seq_len(T)) {
    
    pow <- agents$power   # power from t-1 (round 1: all power = 1)
    
    # --------------------------------------------------
    # Lay agents make their delegation decision
    # --------------------------------------------------
    targets <- vapply(lay_ids, function(i) {
      nb <- adj[[i]]
      
      # Weight for voting directly
      w_self <- 1 / (1 + exp(-responsiveness * pow[i]))
      
      # Weight for delegating to each neighbour j
      w_nb <- if (length(nb))
        compute_attractiveness(pref[i], pref[nb], pow[i], pow[nb],
                               responsiveness)
      else numeric(0)
      
      w <- pmax(c(w_self, w_nb), 0)
      if (sum(w) == 0) w[] <- 1   # fallback: uniform if all weights are 0
      
      # Draw: index 1 = self, index 2+ = neighbours
      c(i, nb)[sample.int(length(w), 1L, prob = w)]
    }, integer(1L))
    
    # --------------------------------------------------
    # Build delegation graph for this round
    # Edge i -> j means: i delegates to j
    # --------------------------------------------------
    mask      <- targets != lay_ids   # only genuine delegations
    edge_from <- lay_ids[mask]        # lay agents who delegate
    edge_to   <- targets[mask]        # target: another lay agent or expert
    
    if (length(edge_from)) {
      gD <- graph_from_edgelist(cbind(edge_from, edge_to), directed = TRUE)
      gD <- add_vertices(gD, max(0, n_all - vcount(gD)))
    } else {
      gD <- make_empty_graph(n = n_all, directed = TRUE)
    }
    delegation_graphs[[t]] <- gD
    
    # --------------------------------------------------
    # Compute transitive power (available to agents in round t+1)
    # --------------------------------------------------
    agents$power <- compute_power(n_all, edge_from, edge_to)
    
    # --------------------------------------------------
    # Propagate votes along delegation chains
    # --------------------------------------------------
    agents$my_vote   <- propagate_votes(pref, n_all, edge_from, edge_to)
    agents$delegated <- seq_len(n_all) %in% edge_from
    
    # Share of lost votes (cycles -> NA)
    history[t] <- mean(is.na(agents$my_vote))
  }
  
  list(
    agents            = agents,
    history_lost      = history,
    delegation_graphs = delegation_graphs,
    final_graph       = delegation_graphs[[T]],
    friendship_graph  = gF
  )
}