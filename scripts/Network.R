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
  r  <- roots[edge_from]
  ok <- !is.na(r)
  votes[edge_from[ok]]  <- opinion[r[ok]]
  votes[edge_from[!ok]] <- NA_real_
  votes
}

# =============================================================
# COMBINED POWER + VOTE PROPAGATION (single root search)
# Runs find_roots_vectorized once and computes both results.
# =============================================================

compute_power_and_votes <- function(opinion, n, edge_from, edge_to) {
  power <- rep(1L, n)
  votes <- opinion
  if (!length(edge_from)) return(list(power = power, votes = votes))

  delegate_of <- integer(n)
  delegate_of[edge_from] <- edge_to
  roots <- find_roots_vectorized(n, delegate_of)

  valid <- edge_from[!is.na(roots[edge_from])]
  if (length(valid)) power <- power + tabulate(roots[valid], nbins = n)

  r  <- roots[edge_from]
  ok <- !is.na(r)
  votes[edge_from[ok]]  <- opinion[r[ok]]
  votes[edge_from[!ok]] <- NA_real_

  list(power = power, votes = votes)
}

# =============================================================
# AGENT SETUP
# =============================================================

setup_agents <- function(n_per_community, n_communities,
                         n_experts_per_community, seed = 1,
                         minority_share = 0) {
  set.seed(seed)
  n_lay <- n_per_community * n_communities
  n_exp <- n_experts_per_community * n_communities
  n_all <- n_lay + n_exp

  n_min   <- round(n_lay * minority_share)
  grp_lay <- c(rep("minority", n_min), rep("majority", n_lay - n_min))

  agents <- tibble(
    id        = 1:n_all,
    type      = c(rep("lay", n_lay), rep("expert", n_exp)),
    opinion   = runif(n_all),
    community = c(
                  (0:(n_lay - 1)) %% n_communities,
                  if (n_exp > 0) (0:(n_exp - 1)) %% n_communities else integer(0)
                ),
    group     = c(grp_lay, rep("majority", n_exp)),
    power     = 1L,
    my_vote   = NA_real_,
    delegated = FALSE
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
                                     p_rewire = 0.05, seed = 1,
                                     p_ingroup = 0) {
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
        if (length(cands)) {
          if (p_ingroup != 0) {
            same_g <- agents$group[cands] == agents$group[u]
            wts    <- ifelse(same_g, exp(p_ingroup), 1)
            edge_list[i, 2] <- sample(cands, 1, prob = wts / sum(wts))
          } else {
            edge_list[i, 2] <- sample(cands, 1)
          }
        }
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
#     Two-step decision:
#       Step 1: vote directly with fixed probability p_self;
#               if no neighbours exist, always vote directly.
#       Step 2: if delegating, choose neighbour j by
#               attractiveness(i,j) = proximity x competence
#               (trust and ingroup modifiers applied here)
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
    p_self                  = 0.5,
    attractiveness_fn       = attractiveness_dual_sigmoid,
    sigma_opinion           = 0,       # SD of logit-space noise on perceived opinion
    sigma_pow               = 0,       # SD of log-space noise on perceived power
    minority_share          = 0,       # fraction of lay agents in minority group
    minority_opinion_mu     = NULL,    # if not NULL: minority opinions ~ N(mu, sigma_m)
    minority_opinion_sigma  = 0.1,     # SD for clustered minority opinions
    r_ingroup               = 0,       # ingroup responsiveness (0 = no preference)
    p_ingroup               = 0,       # structural homophily in network rewiring
    lambda                  = 0,       # trust decay/momentum  (0 = no trust)
    gamma                   = 0,       # trust sensitivity     (0 = trust disabled)
    cycle_fallback          = "none",  # "none" | "direct" | "redelegate"
    target_homophily        = NULL,    # target assortativity; NULL disables shuffle
    homophily_t             = 5,       # Metropolis temperature (higher = more greedy)
    homophily_steps         = 100000   # safety cap on proposed swaps
) {
  st     <- setup_agents(n_per_community, n_communities,
                         n_experts_per_community, seed,
                         minority_share = minority_share)
  agents <- st$agents
  n_all  <- st$n_all

  gF <- setup_friendship_network(agents, n_communities, node_degree,
                                 p_rewire, seed, p_ingroup = p_ingroup)
  gF <- connect_experts(gF, agents, n_per_community,
                        expert_connectedness, seed)

  # Redistribute lay opinions on the fixed network so that connected
  # agents become more similar. No-op when target_homophily is NULL.
  agents <- shuffle_opinions_homophily(agents, gF, target_homophily,
                                       homophily_t, homophily_steps, seed)

  # Condition B: clustered minority opinions (overrides uniform / shuffled opinions)
  if (!is.null(minority_opinion_mu)) {
    min_ids_init <- which(agents$group == "minority")
    agents$opinion[min_ids_init] <- pmin(pmax(
      rnorm(length(min_ids_init), minority_opinion_mu, minority_opinion_sigma),
      0), 1)
  }

  adj     <- lapply(seq_len(n_all),
                    \(v) as.integer(neighbors(gF, v, mode = "out")))
  lay_ids <- which(agents$type == "lay")
  op      <- agents$opinion

  # Trust state — only allocated when trust is active (gamma > 0 or lambda > 0)
  trust_active <- (lambda != 0 || gamma != 0)
  prev_my_vote <- rep(NA_real_, n_all)
  tau <- if (trust_active)
    lapply(adj, function(nb) setNames(rep(0.0, length(nb)), as.character(nb)))
  else NULL

  prev_lost_ids <- integer(0L)   # lay agents whose vote was NA last round
  prev_lost_set <- logical(n_all)
  prev_target   <- integer(n_all) # who each agent delegated to last round (0 = direct)

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

      # Between-round fallback: if vote was lost last round, adjust choice
      if (cycle_fallback != "none" && prev_lost_set[i]) {
        if (cycle_fallback == "direct") return(i)
        if (cycle_fallback == "redelegate") {
          # exclude the specific delegate that caused the cycle last round
          nb <- nb[nb != prev_target[i]]
        } else if (cycle_fallback == "informed") {
          # only consider neighbours whose vote was represented last round
          nb <- nb[!is.na(prev_my_vote[nb])]
          if (!length(nb)) return(i)
        }
      }

      # Step 1: vote directly with fixed probability p_self (always direct if no neighbours)
      if (!length(nb) || runif(1) < p_self) return(i)

      # Step 2: choose neighbour by attractiveness
      op_nb  <- perceive_opinion(op[nb],  sigma_opinion)
      pow_nb <- perceive_power(pow[nb], sigma_pow)

      # Inertia: retain current delegate with probability `inertia`
      if (inertia > 0 && prev_target[i] != 0L && runif(1) < inertia) return(prev_target[i])

      w_nb <- attractiveness_fn(op[i], op_nb, pow[i], pow_nb, r_op, r_pw)

      # Trust modifier: A_tilde = A * 2*sigma(gamma*tau); neutral at tau = 0
      if (trust_active && gamma != 0) {
        trust_mod <- 2 * .sig(gamma * tau[[i]][as.character(nb)])
        w_nb <- w_nb * trust_mod
      }

      # Ingroup modifier: A_tilde *= sigma(r_ingroup * I(g_i == g_j)), I in {-1, +1}
      if (r_ingroup != 0) {
        same_g <- as.integer(agents$group[nb] == agents$group[i]) * 2L - 1L
        w_nb   <- w_nb * .sig(r_ingroup * same_g)
      }

      w <- pmax(w_nb, 0)
      if (sum(w) == 0) w[] <- 1
      nb[sample.int(length(w), 1L, prob = w)]
    }, integer(1L))

    # --------------------------------------------------
    # Build delegation graph
    # --------------------------------------------------
    mask      <- targets != lay_ids
    edge_from <- lay_ids[mask]
    edge_to   <- targets[mask]

    gD <- make_empty_graph(n = n_all, directed = TRUE)
    if (length(edge_from)) gD <- add_edges(gD, as.vector(rbind(edge_from, edge_to)))
    delegation_graphs[[t]] <- gD

    pv             <- compute_power_and_votes(op, n_all, edge_from, edge_to)
    agents$power   <- pv$power
    agents$my_vote <- pv$votes
    delegated_vec  <- logical(n_all)
    if (length(edge_from)) delegated_vec[edge_from] <- TRUE
    agents$delegated <- delegated_vec

    # --------------------------------------------------
    # Trust update: tau_ij(t) = lambda*tau_ij(t-1) - (1-lambda)*|o_i - v_j(t-1)|
    if (trust_active) {
      for (i in lay_ids) {
        nb <- adj[[i]]
        if (!length(nb)) next
        vj    <- prev_my_vote[nb]
        valid <- !is.na(vj)
        if (!any(valid)) next
        nb_ch <- as.character(nb)
        tau[[i]][nb_ch[valid]] <- lambda * tau[[i]][nb_ch[valid]] -
          (1 - lambda) * abs(op[i] - vj[valid])
      }
    }
    prev_my_vote <- agents$my_vote  # always update: used by trust and informed fallback

    # --------------------------------------------------
    # Cheap metrics — recorded every round
    # --------------------------------------------------
    represented            <- agents[!is.na(agents$my_vote), ]
    history_lost[t]        <- mean(is.na(agents$my_vote))
    history_drift[t]       <- if (nrow(represented) > 0)
      mean(abs(represented$opinion - represented$my_vote)) else NA_real_
    history_delegation[t]  <- mean(agents$delegated[lay_ids])

    # Stability: compare prev_target (round t-1) with current edges, before update
    history_stability[t]   <- if (t > 1) {
      target_curr <- integer(n_all)
      target_curr[edge_from] <- edge_to
      mean(prev_target[lay_ids] == target_curr[lay_ids])
    } else NA_real_

    # Update between-round fallback state for next round
    prev_lost_ids <- lay_ids[is.na(agents$my_vote[lay_ids])]
    prev_lost_set <- logical(n_all)
    prev_lost_set[prev_lost_ids] <- TRUE
    prev_target            <- integer(n_all)
    prev_target[edge_from] <- edge_to

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
        n_isolated          <- n_all - length(active_nodes)
        total_components    <- comps$no + n_isolated
      } else {
        largest_voting_bloc <- 1 / n_all
        total_components    <- n_all
      }

      stab <- history_stability[t]

      rep_power <- represented$power

      # ---- Minority metrics ------------------------------------------------
      min_ids_s   <- which(agents$group == "minority")
      min_pop_shr <- length(min_ids_s) / n_all

      if (min_pop_shr > 0 && sum(agents$power) > 0) {
        min_pwr_shr <- sum(agents$power[min_ids_s]) / sum(agents$power)
        RR_m        <- min_pwr_shr / min_pop_shr
      } else {
        min_pwr_shr <- 0; RR_m <- NA_real_
      }

      cross_grp_rate <- if (length(edge_from) > 0)
        mean(agents$group[edge_from] != agents$group[edge_to])
      else 0

      min_vtr <- min_ids_s[!is.na(agents$my_vote[min_ids_s])]
      min_enp <- if (length(min_vtr) > 0) {
        pm <- agents$power[min_vtr]
        if (sum(pm) > 0) 1 / sum((pm / sum(pm))^2) else NA_real_
      } else NA_real_

      min_dlg <- min_ids_s[agents$delegated[min_ids_s]]
      min_chain <- if (length(min_dlg) > 0 && length(roots_snap) > 0 && ecount(gD) > 0) {
        dm  <- distances(gD, v = min_dlg, to = roots_snap, mode = "out")
        cls <- apply(dm, 1, function(d) { fd <- d[is.finite(d)]; if (length(fd)) min(fd) else NA_real_ })
        mean(cls, na.rm = TRUE)
      } else 0

      min_rep   <- agents[min_vtr, ]
      min_drift <- if (nrow(min_rep) > 0)
        mean(abs(min_rep$opinion - min_rep$my_vote)) else NA_real_

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
        minority_power_share        = min_pwr_shr,
        RR_m                        = RR_m,
        cross_group_dlg_rate        = cross_grp_rate,
        minority_enp                = min_enp,
        minority_chain_len          = min_chain,
        minority_drift              = min_drift,
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
