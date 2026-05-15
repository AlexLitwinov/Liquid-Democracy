library(igraph)
library(tibble)

source(here::here("scripts/FunctionVersions.R"))

# =============================================================
# PERCEPTION HELPERS
#
# When sigma = 0, perceived = true (backward compatible).
#
# perceive_opinion: logit-space Gaussian noise → output stays in [0, 1]
#   true_val: numeric vector of true opinions in [0, 1]
#   sigma:    SD of noise in logit space (0 = no noise)
#
# perceive_power: log-space Gaussian noise → output stays positive
#   true_val: numeric vector of true power values (>= 1)
#   sigma:    SD of multiplicative noise in log space (0 = no noise)
# =============================================================

perceive_opinion <- function(true_val, sigma) {
  if (sigma == 0) return(true_val)
  plogis(qlogis(true_val) + rnorm(length(true_val), 0, sigma))
}

perceive_power <- function(true_val, sigma) {
  if (sigma == 0) return(true_val)
  true_val * exp(rnorm(length(true_val), 0, sigma))
}

# =============================================================
# ATTRACTIVENESS FUNCTION
# Ideological proximity x competence sigmoid
# =============================================================

compute_attractiveness <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  .sig(r_op * (1 - 2 * abs(op_i - op_j))) * .sig(r_pw * log(pow_j / pow_i))
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
# Direct voters: keep their own opinion as their vote
# Delegating agents: adopt the opinion of their chain's root
# Cycle agents: vote is lost (NA)
# =============================================================

propagate_votes <- function(opinion, n, edge_from, edge_to) {
  votes <- opinion
  if (!length(edge_from)) return(votes)

  delegate_of <- integer(n)
  delegate_of[edge_from] <- edge_to

  roots <- find_roots_vectorized(n, delegate_of)

  # Delegating agents inherit the opinion of their root
  for (i in edge_from) {
    r        <- roots[i]
    votes[i] <- if (is.na(r)) NA_real_ else opinion[r]
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
    opinion    = runif(n_all),          # fixed ideological position on [0,1]
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
# HOMOPHILY OPINION SHUFFLE (Metropolis)
#
# Runs AFTER the friendship network is built and BEFORE the
# simulation loop. The network structure is never touched —
# only opinions are redistributed among lay agents so that
# connected nodes become more similar.
#
# Algorithm (per step):
#   1. Pick two random lay agents i and j
#   2. Compute current total opinion disagreement on edges
#      touching i or j (the i-j edge itself cancels and is excluded)
#   3. Propose swapping op[i] <-> op[j]
#   4. Accept if disagreement decreases; otherwise accept with
#      probability exp(-delta_E * homophily_t)
#   5. Every 100 steps: check assortativity; stop if target reached
#
# Parameters:
#   target_homophily : target opinion assortativity to reach;
#                      NULL disables the step entirely
#   homophily_t      : Metropolis temperature — higher = more greedy,
#                      faster convergence (default 5)
#   homophily_steps  : safety cap on proposed swaps (default 100 000)
#
# Returns: agents tibble with (possibly) reshuffled opinions
# =============================================================

shuffle_opinions_homophily <- function(agents, gF,
                                       target_homophily,
                                       homophily_t     = 5,
                                       homophily_steps = 100000,
                                       seed            = 1) {
  if (is.null(target_homophily)) return(agents)
  set.seed(seed)

  lay_ids <- which(agents$type == "lay")
  n_lay   <- length(lay_ids)
  op      <- agents$opinion
  gF_lay  <- induced_subgraph(gF, lay_ids)   # lay-only subgraph for assortativity

  for (step in seq_len(homophily_steps)) {

    # Check assortativity every 100 steps; stop when target is reached
    if (step %% 100L == 0L) {
      if (assortativity(gF_lay, op[lay_ids], directed = FALSE) >= target_homophily)
        break
    }

    idx <- sample(n_lay, 2)
    i   <- lay_ids[idx[1]]
    j   <- lay_ids[idx[2]]

    # Lay neighbours of i and j, excluding the i-j edge (cancels in delta_E)
    nb_i <- setdiff(intersect(as.integer(neighbors(gF, i, mode = "out")), lay_ids), j)
    nb_j <- setdiff(intersect(as.integer(neighbors(gF, j, mode = "out")), lay_ids), i)

    E_before <- sum(abs(op[i] - op[nb_i])) + sum(abs(op[j] - op[nb_j]))
    E_after  <- sum(abs(op[j] - op[nb_i])) + sum(abs(op[i] - op[nb_j]))
    delta_E  <- E_after - E_before

    if (delta_E < 0 || runif(1) < exp(-delta_E * homophily_t)) {
      op[c(i, j)] <- op[c(j, i)]
    }
  }

  agents$opinion <- op
  agents
}

# =============================================================
# NETWORK HOMOPHILY MEASUREMENT
#
# Computes two complementary measures of opinion homophily on
# the lay-to-lay subgraph of the friendship network:
#
#   assortativity         — Pearson correlation of opinions across
#                           connected lay pairs; range [-1, 1];
#                           0 = random baseline, 1 = perfect homophily
#
#   mean_edge_disagreement — mean |op_i - op_j| over all lay-to-lay
#                            edges; range [0, 1]; random baseline ≈ 0.33;
#                            directly proportional to the energy E that
#                            the Metropolis shuffle minimises
#
# Usage: compute_network_homophily(res$friendship_graph, res$agents)
# =============================================================

compute_network_homophily <- function(gF, agents) {
  lay_ids <- which(agents$type == "lay")
  op      <- agents$opinion
  gF_lay  <- induced_subgraph(gF, lay_ids)
  lay_op  <- op[lay_ids]

  assort <- assortativity(gF_lay, lay_op, directed = FALSE)

  el     <- as_edgelist(gF_lay, names = FALSE)
  mean_d <- if (nrow(el) > 0)
    mean(abs(lay_op[el[, 1]] - lay_op[el[, 2]]))
  else NA_real_

  list(
    assortativity          = round(assort,  4),
    mean_edge_disagreement = round(mean_d,  4)
  )
}

# =============================================================
# MAIN SIMULATION
#
# Each of the T rounds:
#
#   LAY AGENTS:
#     Observe the power and opinion of all neighbours from t-1,
#     subject to perception noise (sigma_opinion, sigma_pow).
#     Setting both sigmas to 0 (default) reproduces the original
#     model where perceived = true values.
#
#     Choose probabilistically:
#       (a) Vote directly (target = self)
#           Weight: selfweight_fn(i, nb, op, pow, responsiveness)
#       (b) Delegate to neighbour j
#           Weight: attractiveness(i,j) = proximity x competence
#
#   EXPERTS:
#     Always vote directly with their own opinion.
#     Do not participate in delegation.
#
#   AFTER EACH ROUND:
#     Power is computed transitively.
#     Votes propagate along chains to the root.
# =============================================================
simulate_liquid_democracy <- function(
    seed                    = 123,
    n_per_community         = 250,
    n_communities           = 1,
    node_degree             = 6,
    n_experts_per_community = 0,
    expert_connectedness    = 0,
    p_rewire                = 0.05,
    r_op                    = 1,
    r_pw                    = 1,
    inertia                 = 0,
    T                       = 200,
    selfweight_fn           = selfweight_dual_sigmoid,
    attractiveness_fn       = attractiveness_dual_sigmoid,
    sigma_opinion           = 0,       # SD of logit-space noise on perceived opinion
    sigma_pow               = 0,       # SD of log-space noise on perceived power
    target_homophily        = NULL,    # target assortativity; NULL disables shuffle
    homophily_t             = 5,       # Metropolis temperature (higher = more greedy)
    homophily_steps         = 100000   # safety cap on proposed swaps
) {
  st     <- setup_agents(n_per_community, n_communities,
                         n_experts_per_community, seed)
  agents <- st$agents
  n_all  <- st$n_all

  gF <- setup_friendship_network(agents, n_communities, node_degree,
                                 p_rewire, seed)
  gF <- connect_experts(gF, agents, n_per_community,
                        expert_connectedness, seed)

  # Redistribute lay opinions on the fixed network so that connected
  # agents become more similar. No-op when target_homophily is NULL.
  agents <- shuffle_opinions_homophily(agents, gF, target_homophily,
                                       homophily_t, homophily_steps, seed)

  adj     <- lapply(seq_len(n_all),
                    \(v) as.integer(neighbors(gF, v, mode = "out")))
  lay_ids <- which(agents$type == "lay")
  op      <- agents$opinion

  # ---------------------------------------------------------
  # Snapshot rounds: 25%, 50%, 75%, 100% of T
  # ---------------------------------------------------------
  snapshot_rounds <- unique(round(c(0.25, 0.50, 0.75, 1.00) * T))

  history_lost       <- numeric(T)
  history_drift      <- numeric(T)
  history_delegation <- numeric(T)
  history_stability  <- numeric(T)

  snapshot_list     <- vector("list", length(snapshot_rounds))
  names(snapshot_list) <- as.character(snapshot_rounds)

  delegation_graphs <- vector("list", T)

  # Helper: Gini coefficient
  gini <- function(x) {
    if (sum(x) == 0) return(0)
    x <- sort(x); n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }

  # Helper: Top-5% power share
  top5 <- function(x) {
    n_top <- max(1, floor(0.05 * length(x)))
    sum(sort(x, decreasing = TRUE)[1:n_top]) / sum(x)
  }

  for (t in seq_len(T)) {

    pow <- agents$power

    # --------------------------------------------------
    # Delegation decision
    # --------------------------------------------------
    targets <- vapply(lay_ids, function(i) {
      nb <- adj[[i]]

      # Perceived values for neighbours (own values always true)
      op_nb  <- perceive_opinion(op[nb],  sigma_opinion)
      pow_nb <- perceive_power(pow[nb], sigma_pow)

      # Build perceived vectors (only neighbour slots replaced)
      op_perc  <- replace(op,  nb, op_nb)
      pow_perc <- replace(pow, nb, pow_nb)

      w_self <- selfweight_fn(i, nb, op_perc, pow_perc, r_op, r_pw)

      # Inertia: retain current delegate with probability `inertia`
      if (inertia > 0 && agents$delegated[i]) {
        el      <- as_edgelist(delegation_graphs[[max(1, t - 1)]], names = FALSE)
        matches <- el[el[, 1] == i, 2]
        if (length(matches) && runif(1) < inertia) return(matches[1])
      }

      w_nb <- if (length(nb))
        attractiveness_fn(op[i], op_nb, pow[i], pow_nb, r_op, r_pw)
      else numeric(0)

      w <- pmax(c(w_self, w_nb), 0)
      if (sum(w) == 0) w[] <- 1
      c(i, nb)[sample.int(length(w), 1L, prob = w)]
    }, integer(1L))

    # --------------------------------------------------
    # Build delegation graph
    # --------------------------------------------------
    mask      <- targets != lay_ids
    edge_from <- lay_ids[mask]
    edge_to   <- targets[mask]

    if (length(edge_from)) {
      gD <- graph_from_edgelist(cbind(edge_from, edge_to), directed = TRUE)
      gD <- add_vertices(gD, max(0, n_all - vcount(gD)))
    } else {
      gD <- make_empty_graph(n = n_all, directed = TRUE)
    }
    delegation_graphs[[t]] <- gD

    agents$power     <- compute_power(n_all, edge_from, edge_to)
    agents$my_vote   <- propagate_votes(op, n_all, edge_from, edge_to)
    agents$delegated <- seq_len(n_all) %in% edge_from

    # --------------------------------------------------
    # Cheap metrics — recorded every round
    # --------------------------------------------------
    represented            <- agents[!is.na(agents$my_vote), ]
    history_lost[t]        <- mean(is.na(agents$my_vote))
    history_drift[t]       <- if (nrow(represented) > 0)
      mean(abs(represented$opinion - represented$my_vote)) else NA_real_
    history_delegation[t]  <- mean(agents$delegated[agents$type == "lay"])

    history_stability[t]   <- if (t > 1) {
      el_prev     <- as_edgelist(delegation_graphs[[t - 1]], names = FALSE)
      el_curr     <- as_edgelist(gD, names = FALSE)
      target_prev <- integer(n_all)
      target_curr <- integer(n_all)
      if (nrow(el_prev)) target_prev[el_prev[, 1]] <- el_prev[, 2]
      if (nrow(el_curr)) target_curr[el_curr[, 1]] <- el_curr[, 2]
      mean(target_prev[lay_ids] == target_curr[lay_ids])
    } else NA_real_

    # --------------------------------------------------
    # Full snapshot — recorded at 25%, 50%, 75%, 100% of T
    # --------------------------------------------------
    if (t %in% snapshot_rounds) {

      roots_snap <- which(degree(gD, mode = "out") == 0)
      if (length(roots_snap) > 0 && ecount(gD) > 0) {
        dist_mat      <- distances(gD, mode = "out", to = roots_snap)
        chain_lengths <- apply(dist_mat, 1, function(d) {
          fd <- d[is.finite(d)]; if (length(fd)) min(fd) else NA_real_
        })
        delegating_lengths          <- chain_lengths[!is.na(chain_lengths) & chain_lengths > 0]
        avg_chain_length_delegators <- if (length(delegating_lengths) > 0)
          mean(delegating_lengths) else 0
      } else {
        avg_chain_length_delegators <- 0
      }

      active_nodes <- which(degree(gD, mode = "all") > 0)
      if (length(active_nodes) > 0) {
        gD_active           <- induced_subgraph(gD, active_nodes)
        comps               <- components(gD_active)
        largest_voting_bloc <- max(comps$csize) / n_all
        total_components    <- comps$no
      } else {
        largest_voting_bloc <- 0
        total_components    <- 0
      }

      stab <- if (t > 1) {
        g_prev      <- delegation_graphs[[t - 1]]
        el_prev     <- as_edgelist(g_prev, names = FALSE)
        el_curr     <- as_edgelist(gD,     names = FALSE)

        target_prev <- integer(n_all)
        target_curr <- integer(n_all)

        if (nrow(el_prev)) target_prev[el_prev[, 1]] <- el_prev[, 2]
        if (nrow(el_curr)) target_curr[el_curr[, 1]] <- el_curr[, 2]

        mean(target_prev[lay_ids] == target_curr[lay_ids])
      } else NA_real_

      rep_power <- represented$power

      snapshot_list[[as.character(t)]] <- tibble(
        round                       = t,
        pct_T                       = t / T,
        lost_vote_rate              = history_lost[t],
        avg_drift                   = history_drift[t],
        delegation_rate             = history_delegation[t],
        gini_power                  = gini(rep_power),
        top5_power_share            = top5(rep_power),
        avg_chain_length_delegators = avg_chain_length_delegators,
        largest_voting_bloc_share   = largest_voting_bloc,
        total_components            = total_components,
        delegation_stability        = stab,
        direct_yes                  = sum(agents$opinion >= 0.5),
        direct_no                   = sum(agents$opinion  < 0.5),
        direct_margin               = sum(agents$opinion >= 0.5) -
          sum(agents$opinion  < 0.5),
        liquid_yes                  = sum(represented$my_vote >= 0.5, na.rm = TRUE),
        liquid_no                   = sum(represented$my_vote  < 0.5, na.rm = TRUE),
        liquid_margin               = sum(represented$my_vote >= 0.5, na.rm = TRUE) -
          sum(represented$my_vote  < 0.5, na.rm = TRUE)
      )
    }
  }

  list(
    agents             = agents,
    history_lost       = history_lost,
    history_drift      = history_drift,
    history_delegation = history_delegation,
    history_stability  = history_stability,
    snapshots          = bind_rows(snapshot_list),
    delegation_graphs  = delegation_graphs,
    final_graph        = delegation_graphs[[T]],
    friendship_graph   = gF
  )
}
