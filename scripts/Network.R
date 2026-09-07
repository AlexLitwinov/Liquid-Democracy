library(igraph)
library(tibble)

source(here::here("scripts/FunctionVersions.R"))

# =============================================================
# PERCEPTION HELPERS
#
# perceive_opinion: logit-space Gaussian noise on observed neighbour opinions.
#   true_val: numeric vector of true opinions in [0, 1]
#   sigma:    SD of noise in logit space (0 = no noise, default)
# =============================================================

perceive_opinion <- function(true_val, sigma) {
  if (sigma == 0) return(true_val)
  plogis(qlogis(true_val) + rnorm(length(true_val), 0, sigma))
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
# CYCLE LENGTH BREAKDOWN (RQ1 -- how many agents are stuck in a 2-cycle
# specifically, vs. longer cycles)
#
# in_cycle (see find_roots_vectorized above) flags every agent whose chain
# never reaches a root -- that includes genuine cycle MEMBERS but also
# "tail" agents who merely feed into a cycle without looping back to
# themselves (e.g. 1 -> 29 -> 26 -> 23 -> [23<->24<->25 cycle], forever --
# 1/29/26 never revisit themselves). Naively following delegate_of from
# such a tail agent until it equals `start` again never terminates. Fixed
# here by bounding each traversal at n_cand steps (more than enough to
# complete any genuine cycle, since a cycle can involve at most all
# candidates) and only recording a cycle_length when the walk actually
# closes back on `start`; tail agents get NA (their vote is still lost --
# counted in lost_vote_rate as before -- they're just not a cycle member,
# so no single cycle_length applies to them).
#
# Returns an integer vector of length n: cycle length for genuine cycle
# members, NA for everyone else (roots, direct voters, and tail agents).
# =============================================================

compute_cycle_lengths <- function(n, delegate_of, in_cycle) {
  cyc_len <- rep(NA_integer_, n)
  visited <- rep(FALSE, n)
  cand    <- which(in_cycle)
  n_cand  <- length(cand)
  for (start in cand) {
    if (visited[start]) next
    path  <- start
    cur   <- delegate_of[start]
    steps <- 1L
    closed <- FALSE
    while (steps <= n_cand) {
      if (cur == start) { closed <- TRUE; break }
      path  <- c(path, cur)
      cur   <- delegate_of[cur]
      steps <- steps + 1L
    }
    if (closed) {
      visited[path] <- TRUE
      cyc_len[path] <- length(path)
    } else {
      visited[start] <- TRUE   # tail agent -- leave cyc_len[start] = NA
    }
  }
  cyc_len
}

# =============================================================
# POWER COMPUTATION (vectorized)
# Every agent starts with power = 1 (their own vote).
# Expose Sec 2.2.7: p_i = 1 + sum_{j->i} p_j -- power accumulates
# transitively along the WHOLE chain, so every ancestor of a delegator
# gains, not just the chain's terminal root (e.g. i -> j -> k gives
# p_i=1, p_j=2, p_k=3; see accumulate_chain_power() below).
# Cycles: members' power is overwritten with their cycle's size below.
# =============================================================

# =============================================================
# CHAIN POWER ACCUMULATION (vectorized)
#
# Credits +1 to EVERY ancestor along each valid (non-cycle) delegator's
# path to its root -- not just the terminal root -- via the same
# pointer-jumping technique as find_roots_vectorized() above. This is
# what makes an intermediate node (one who delegates further themselves)
# show accumulated power to whoever evaluates it as a delegation target
# next round, matching the Expose's recursive p_i = 1 + sum_{j->i} p_j
# exactly (verified against the i->j->k worked example: p_i=1, p_j=2,
# p_k=3, and against branching trees).
# =============================================================
accumulate_chain_power <- function(n, delegate_of, valid, power) {
  if (!length(valid)) return(power)
  pointer <- delegate_of[valid]
  active  <- rep(TRUE, length(valid))
  for (iter in seq_len(n)) {
    if (!any(active)) break
    contrib <- tabulate(pointer[active], nbins = n)
    power   <- power + contrib
    nxt        <- delegate_of[pointer[active]]
    still      <- nxt != 0L
    idx_active <- which(active)
    active[idx_active[!still]] <- FALSE
    pointer[idx_active[still]] <- nxt[still]
  }
  power
}

compute_power <- function(n, edge_from, edge_to) {
  power <- rep(1L, n)
  if (!length(edge_from)) return(power)

  delegate_of <- integer(n)
  delegate_of[edge_from] <- edge_to

  roots <- find_roots_vectorized(n, delegate_of)

  # Expose Sec 2.2.7: every ancestor along a valid (non-cycle) delegator's
  # chain accumulates power, not just the chain's terminal root.
  valid <- edge_from[!is.na(roots[edge_from])]
  power <- accumulate_chain_power(n, delegate_of, valid, power)

  # Sec 2.2.7: for a delegation cycle, the recursion never terminates, so
  # "each agent in a loop is... assigned a voting power equal to the number
  # of agents in the corresponding subgraph" -- overwrite genuine cycle
  # members' power with their cycle's size (tail agents that merely feed
  # into a cycle without looping back to themselves keep the baseline
  # power = 1, since the Expose only specifies the rule for agents actually
  # "involved in the loop"; see compute_cycle_lengths()'s docstring).
  in_cycle <- is.na(roots)
  if (any(in_cycle)) {
    cyc_len <- compute_cycle_lengths(n, delegate_of, in_cycle)
    has_len <- !is.na(cyc_len)
    power[has_len] <- cyc_len[has_len]
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
  power   <- rep(1L, n)
  votes   <- opinion
  roots   <- seq_len(n)  # no delegations: everyone is their own root
  cyc_len <- rep(NA_integer_, n)
  if (!length(edge_from)) return(list(power = power, votes = votes, roots = roots, cyc_len = cyc_len))

  delegate_of <- integer(n)
  delegate_of[edge_from] <- edge_to
  roots <- find_roots_vectorized(n, delegate_of)

  # Expose Sec 2.2.7: every ancestor along a valid (non-cycle) delegator's
  # chain accumulates power, not just the chain's terminal root -- see
  # accumulate_chain_power() above compute_power().
  valid <- edge_from[!is.na(roots[edge_from])]
  power <- accumulate_chain_power(n, delegate_of, valid, power)

  r  <- roots[edge_from]
  ok <- !is.na(r)
  votes[edge_from[ok]]  <- opinion[r[ok]]
  votes[edge_from[!ok]] <- NA_real_

  # Sec 2.2.7 cycle-power rule -- see compute_power() above for the full
  # rationale. cyc_len is also returned so callers (the main round loop)
  # don't need to recompute it a second time for cycle-length diagnostics.
  in_cycle <- is.na(roots)
  if (any(in_cycle)) {
    cyc_len <- compute_cycle_lengths(n, delegate_of, in_cycle)
    has_len <- !is.na(cyc_len)
    power[has_len] <- cyc_len[has_len]
  }

  list(power = power, votes = votes, roots = roots, cyc_len = cyc_len)
}

# =============================================================
# AGENT SETUP
# =============================================================

setup_agents <- function(n_per_community, n_communities, seed = 1,
                         minority_share = 0) {
  set.seed(seed)
  n_all <- n_per_community * n_communities
  n_min <- round(n_all * minority_share)

  # Group is assigned to a RANDOM subset of ids, not a contiguous id block --
  # the friendship network below connects agents by id-adjacency (a ring), so
  # a contiguous id block would make group membership almost perfectly
  # predictable from network position (near-total structural in-group
  # clustering under every condition, not just "homophily"). Randomising the
  # id<->group mapping keeps network structure independent of group for
  # "random"/"polarised", as intended (see Report 15 Sec 1.2 / Report 19).
  agents <- tibble(
    id        = 1:n_all,
    type      = rep("lay", n_all),
    opinion   = runif(n_all),
    community = (0:(n_all - 1)) %% n_communities,
    group     = sample(c(rep("minority", n_min), rep("majority", n_all - n_min))),
    power     = 1L,
    my_vote   = NA_real_,
    delegated = FALSE
  )
  list(agents = agents, n_lay = n_all, n_all = n_all)
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
# =============================================================

setup_friendship_network <- function(agents, n_communities, node_degree,
                                     p_rewire = 0.05, seed = 1,
                                     network_type = "ws") {
  # network_type = "ws" (default, unchanged behaviour): Watts-Strogatz ring
  #   + rewiring, as above.
  # network_type = "ba": Barabasi-Albert preferential attachment (RQ1 --
  #   tests the "rich-gets-richer" dynamic against r_pw, which WS cannot
  #   produce regardless of p_rewire since its degree distribution stays
  #   narrow/homogeneous). p_rewire is ignored in this branch -- BA's
  #   growth process is itself the source of degree heterogeneity, there
  #   is no "rewire" analogue. Mean degree still targets node_degree via
  #   m = node_degree/2 edges attached per new vertex, for comparability
  #   with the WS variant.
  stopifnot(node_degree %% 2 == 0)
  stopifnot(network_type %in% c("ws", "ba"))
  set.seed(seed)

  lay_ids <- which(agents$type == "lay")
  k       <- node_degree %/% 2L

  if (network_type == "ba") {
    edge_list <- do.call(rbind, lapply(0:(n_communities - 1), function(com) {
      nc  <- sort(lay_ids[agents$community[lay_ids] == com])
      n_c <- length(nc)
      if (n_c < 2) return(NULL)
      m     <- max(1L, min(k, n_c - 1L))
      g_ba  <- sample_pa(n_c, power = 1, m = m, directed = FALSE)
      el    <- as_edgelist(g_ba, names = FALSE)
      cbind(nc[el[, 1]], nc[el[, 2]])
    }))
  } else {
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
#   5. Every 100 steps: check GROUP-SPECIFIC assortativity; stop only
#      once BOTH the minority-only and majority-only subgraphs have
#      individually reached target_homophily (see note below)
#
# Parameters:
#   target_homophily : target opinion assortativity to reach, checked
#                      separately within each group's own induced
#                      subgraph; NULL disables the step entirely
#   homophily_t      : Metropolis temperature — higher = more greedy,
#                      faster convergence (default 5)
#   homophily_steps  : safety cap on proposed swaps (default 400 000)
#
# Why group-specific, not pooled: stopping once the POOLED assortativity
# (all lay agents together) reaches target_homophily was found to satisfy
# the pooled target almost entirely on the back of the majority group,
# leaving the minority far short of it -- e.g. at target_homophily = 0.8,
# 30 diagnostic seeds gave pooled r ≈ 0.80 and majority-only r ≈ 0.75, but
# minority-only r ≈ 0.36. Majority-majority edges vastly outnumber
# minority-minority ones under an 80/20 split, so the pooled statistic is
# dominated by the majority and can hit target while the minority
# subgraph is left substantially less homophilous. Checking both groups'
# own assortativity directly fixes this at the cost of more steps, since
# minority-relevant swaps are proposed less often under uniform sampling
# over all lay agents (see scripts/diagnose_group_homophily.R).
#
# Returns: agents tibble with (possibly) reshuffled opinions
# =============================================================

shuffle_opinions_homophily <- function(agents, gF,
                                       target_homophily,
                                       homophily_t     = 5,
                                       homophily_steps = 400000,
                                       seed            = 1) {
  if (is.null(target_homophily)) return(agents)
  set.seed(seed)

  lay_ids <- which(agents$type == "lay")
  n_lay   <- length(lay_ids)
  op      <- agents$opinion
  grp     <- agents$group   # moved together with op below -- see note
  gF_lay  <- induced_subgraph(gF, lay_ids)   # lay-only subgraph for assortativity

  # Precompute each lay agent's lay-only neighbour list ONCE. Network
  # topology (gF) is fixed for the rest of this function -- only opinions
  # and group are being reshuffled across existing positions -- so
  # re-querying igraph's neighbors() twice per step, for up to
  # homophily_steps (default 400 000) steps, was pure repeated overhead
  # for a value that never changes. Purely deterministic (no RNG
  # involved), so this changes none of the random draws below.
  nb_list <- vector("list", max(lay_ids))
  for (v in lay_ids) {
    nb_list[[v]] <- intersect(as.integer(neighbors(gF, v, mode = "out")), lay_ids)
  }

  # Group-specific assortativity, checked against gF_lay's LOCAL vertex
  # numbering (vertex k of gF_lay == lay_ids[k], since induced_subgraph
  # preserves the order of the vids argument) -- avoids rebuilding a
  # lay_ids lookup table on every check.
  group_targets_met <- function() {
    grp_lay <- grp[lay_ids]
    op_lay  <- op[lay_ids]
    min_pos <- which(grp_lay == "minority")
    maj_pos <- which(grp_lay == "majority")
    if (length(min_pos) < 2 || length(maj_pos) < 2) return(TRUE)  # nothing to check
    a_min <- assortativity(induced_subgraph(gF_lay, min_pos), op_lay[min_pos], directed = FALSE)
    a_maj <- assortativity(induced_subgraph(gF_lay, maj_pos), op_lay[maj_pos], directed = FALSE)
    !is.nan(a_min) && !is.nan(a_maj) && a_min >= target_homophily && a_maj >= target_homophily
  }

  for (step in seq_len(homophily_steps)) {

    # Check group-specific assortativity every 100 steps; stop once both
    # groups individually reach the target
    if (step %% 100L == 0L) {
      if (group_targets_met())
        break
    }

    idx <- sample(n_lay, 2)
    i   <- lay_ids[idx[1]]
    j   <- lay_ids[idx[2]]

    # Lay neighbours of i and j, excluding the i-j edge (cancels in delta_E)
    nb_i <- setdiff(nb_list[[i]], j)
    nb_j <- setdiff(nb_list[[j]], i)

    E_before <- sum(abs(op[i] - op[nb_i])) + sum(abs(op[j] - op[nb_j]))
    E_after  <- sum(abs(op[j] - op[nb_i])) + sum(abs(op[i] - op[nb_j]))
    delta_E  <- E_after - E_before

    if (delta_E < 0 || runif(1) < exp(-delta_E * homophily_t)) {
      # Swap the WHOLE agent (opinion + group), not just the opinion value --
      # this is a re-seating of agents onto fixed network positions, so every
      # per-agent attribute has to move together. Swapping op alone would
      # leave group behind at the old position, decoupling the (moved)
      # opinion from the group it was drawn for -- silently breaking the
      # opinion-correlation-implies-group-clustering mechanism this
      # condition relies on (see Report 19 Sec 5).
      op[c(i, j)]  <- op[c(j, i)]
      grp[c(i, j)] <- grp[c(j, i)]
    }
  }

  agents$opinion <- op
  agents$group   <- grp
  if (!group_targets_met())
    message(sprintf(
      "shuffle_opinions_homophily(): group-specific target r=%.3f not reached by both groups within %d steps",
      target_homophily, homophily_steps))
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
#   Each agent observes the opinion of all neighbours from t-1,
#   subject to optional perception noise (sigma_opinion).
#
#   Two-step decision (endogenous self-weight), exact match to the
#   Expose's Sec 2.2 formulas (attractiveness_fn defaults to
#   attractiveness_expose; Mobius self-vote p_self_mobius_M with M = 4;
#   see FunctionVersions.R sections (A)/(D) -- other, older formula
#   versions are kept there for reference/testing but are no longer
#   switchable via a decision_model argument):
#     Step 1: find the most attractive neighbour j* by
#             attractiveness(i,j) = proximity x competence
#             (trust and ingroup modifiers applied here);
#             if no neighbours exist, always vote directly.
#     Step 2: derive a self-vote probability from j*'s attractiveness
#             (p_self_mobius_M, knob self_reliance_c) and vote directly
#             with that probability; otherwise delegate to a neighbour
#             sampled proportional to attractiveness.
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
    p_rewire                = 0.05,
    network_type            = "ws",    # "ws" (default, unchanged) | "ba" -- see setup_friendship_network()
    r_op                    = 1,
    r_pw                    = 1,
    T                       = 200,
    attractiveness_fn       = attractiveness_expose, # advanced override for tests/comparisons only --
                                        # the active model is always the Expose-Sec-2.2 self-vote
                                        # formula (p_self_mobius_M(..., M = 4)) below regardless of
                                        # what's plugged in here; see FunctionVersions.R section (A)
                                        # for the other archived formulas (no longer switchable via
                                        # a decision_model argument -- removed, see git history for
                                        # the "legacy"/"gaussian_mobius"/"calib" branches this once had)
    sigma_opinion           = 0,       # SD of logit-space noise on perceived opinion; 0 = exact, "auto" = sd(agents$opinion) for this run
    minority_share          = 0,       # fraction of lay agents in minority group
    opinion_dist            = "normal", # "normal" | "uniform" -- distribution family for clustered group opinions below
    minority_opinion_mu     = NULL,    # if not NULL: minority opinions are clustered. "normal": ~ N(mu, sigma_m). "uniform": mu is cosmetic (midpoint); actual draw uses minority_opinion_min/max
    minority_opinion_sigma  = 0.1,     # SD for clustered minority opinions ("normal" only)
    minority_opinion_min    = 0,       # lower bound for clustered minority opinions ("uniform" only)
    minority_opinion_max    = 1,       # upper bound for clustered minority opinions ("uniform" only)
    majority_opinion_mu     = NULL,    # if not NULL: majority opinions are clustered. "normal": ~ N(mu, sigma_M). "uniform": mu is cosmetic (midpoint); actual draw uses majority_opinion_min/max
    majority_opinion_sigma  = 0.1,     # SD for clustered majority opinions ("normal" only)
    majority_opinion_min    = 0,       # lower bound for clustered majority opinions ("uniform" only)
    majority_opinion_max    = 1,       # upper bound for clustered majority opinions ("uniform" only)
    r_ingroup               = 0,       # ingroup responsiveness (0 = no preference)
    self_reliance_c         = 0.5,     # c in the Mobius P_self curve (p_self_mobius_M, M = 4):
                                        # self-vote probability when the best neighbour is exactly as attractive
                                        # as agent i itself
    lambda                  = 0,       # trust decay/momentum ("punish_only"/"reward_punish") OR
                                        # adaptation RATE toward the round's target ("relaxation" --
                                        # opposite role: lambda=1 fully adopts the new target each
                                        # round, lambda=0 never updates). (0 = no trust, any mode)
    gamma                   = 0,       # trust sensitivity r_tr in a_tr = 2*L(gamma*(tau-1 or tau))
    trust_mode              = "punish_only", # "punish_only" | "reward_punish" | "relaxation" — see trust-update block
    cycle_penalty           = 1,       # kappa_cyc: penalty for a non-delivering (cyclic) neighbour, "reward_punish" only
    k1                      = 1,       # "relaxation" only: scales the agreement signal s = k1*(1-2|o_i-v_j|)
    k2                      = 1,       # "relaxation" only: penalty magnitude s = -k2 when j's vote is lost to a cycle
    cycle_fallback          = "none",  # "none" | "direct" | "redelegate"
    target_homophily        = NULL,    # target assortativity; NULL disables shuffle
    homophily_t             = 5,       # Metropolis temperature (higher = more greedy)
    homophily_steps         = 400000,  # safety cap on proposed swaps (raised from 100k: the
                                        # group-aware stopping criterion in shuffle_opinions_homophily()
                                        # needs more steps since minority-relevant swaps are proposed
                                        # less often under uniform sampling -- see that function's docstring)
    snap_rounds             = NULL,    # rounds to record full snapshots; NULL = last 5
    fast_path               = FALSE,   # opt-in vectorised delegation-decision step (see
                                        # "Delegation decision" block below). Default FALSE
                                        # preserves the exact original per-agent loop --
                                        # and therefore exact reproducibility of every
                                        # existing report's cached results -- for any
                                        # caller that doesn't explicitly ask for it. Only
                                        # takes effect when cycle_fallback == "none";
                                        # otherwise silently falls back to the original loop.
    n_voting_rounds         = NULL     # Expose Sec 1.4.1: T rounds split into "delegating
                                        # rounds" (decision + power/vote resolution only) and
                                        # "voting rounds" (same, PLUS the trust update, Sec
                                        # 2.2.2 -- trust only ever changes at a voting round).
                                        # NULL (default): every round is a voting round, i.e.
                                        # the ORIGINAL behaviour (trust updates every round,
                                        # unchanged for any existing caller that doesn't pass
                                        # this). Set to an integer n <= T to instead treat only
                                        # n rounds, evenly spaced with the last one always at
                                        # round T, as voting rounds; trust is frozen on the
                                        # other T - n "delegating/adaptation" rounds. Never
                                        # hard-wired: pass n_voting_rounds = 20 with T = 60 for
                                        # the 60/20/40 standard case.
) {
  sim_time_start <- Sys.time()  # covers the whole call: network/agent setup + all T rounds

  # ---------------------------------------------------------
  # Delegating rounds vs. voting rounds (Expose Sec 1.4.1).
  # n_voting_rounds = NULL: every round is a voting round -- exactly the
  # original behaviour, so any existing caller that doesn't pass this gets
  # identical results to before. Otherwise: n_voting_rounds rounds, evenly
  # spaced with the last one always landing on round T (so the simulation
  # always ends on a "locked in" vote, not mid-adaptation), are voting
  # rounds; trust is only updated on those (see trust-update block below).
  # ---------------------------------------------------------
  voting_rounds <- if (is.null(n_voting_rounds)) {
    seq_len(T)
  } else {
    stopifnot(n_voting_rounds >= 1, n_voting_rounds <= T)
    sort(unique(round(seq(T / n_voting_rounds, T, length.out = n_voting_rounds))))
  }
  is_voting_round <- logical(T)
  is_voting_round[voting_rounds] <- TRUE

  st     <- setup_agents(n_per_community, n_communities, seed,
                         minority_share = minority_share)
  agents <- st$agents
  n_all  <- st$n_all

  gF <- setup_friendship_network(agents, n_communities, node_degree,
                                 p_rewire, seed, network_type)

  # Condition B: clustered minority/majority opinions -- applied BEFORE the
  # homophily shuffle below (not after), so the shuffle rearranges these
  # group-differentiated opinions across the network instead of being
  # overwritten by them. With target_homophily also set, this is what lets
  # opinion-based network clustering indirectly cluster by group too,
  # since group and opinion are correlated once this block runs.
  if (!is.null(minority_opinion_mu)) {
    min_ids_init <- which(agents$group == "minority")
    agents$opinion[min_ids_init] <- if (opinion_dist == "uniform") {
      runif(length(min_ids_init), minority_opinion_min, minority_opinion_max)
    } else pmin(pmax(
      rnorm(length(min_ids_init), minority_opinion_mu, minority_opinion_sigma),
      0), 1)
  }
  if (!is.null(majority_opinion_mu)) {
    maj_ids_init <- which(agents$group == "majority")
    agents$opinion[maj_ids_init] <- if (opinion_dist == "uniform") {
      runif(length(maj_ids_init), majority_opinion_min, majority_opinion_max)
    } else pmin(pmax(
      rnorm(length(maj_ids_init), majority_opinion_mu, majority_opinion_sigma),
      0), 1)
  }

  # Redistribute lay opinions on the fixed network so that connected
  # agents become more similar. No-op when target_homophily is NULL.
  agents <- shuffle_opinions_homophily(agents, gF, target_homophily,
                                       homophily_t, homophily_steps, seed)

  # sigma_opinion = "auto": derive the perception-noise SD from this run's
  # own opinion population (empirical SD of agents$opinion around its
  # mean) instead of a hand-picked constant -- computed once here, since
  # opinions are fixed at initialisation and never change over T.
  if (identical(sigma_opinion, "auto")) {
    sigma_opinion <- sd(agents$opinion)
  }

  adj     <- lapply(seq_len(n_all),
                    \(v) as.integer(neighbors(gF, v, mode = "out")))
  lay_ids <- which(agents$type == "lay")
  op      <- agents$opinion
  grp     <- agents$group  # hoisted out of the round loop: group never changes
                            # over T, so re-deriving it from the tibble every
                            # agent every round (agents$group[nb]) was pure
                            # repeated overhead for an unchanging plain vector.

  # Trust state — only allocated when trust is active (gamma > 0 or lambda > 0)
  trust_active <- (lambda != 0 || gamma != 0)
  prev_my_vote <- rep(NA_real_, n_all)
  # tau[[i]][k] tracks trust toward adj[[i]][k] -- positional, not keyed by
  # neighbour id, since adj (and therefore each agent's neighbour order) is
  # fixed for the whole simulation. Avoids character-vector hash lookups in
  # the hot loop below.
  # Initial value: 0 for "punish_only"/"reward_punish" (neutral point of the
  # OLD a_trust = 2*sig(gamma*tau)); 1 for "relaxation" (neutral point of
  # a_tr = 2*sig(gamma*(tau-1)) -- see attractiveness_calib()'s docstring).
  tau_init <- if (identical(trust_mode, "relaxation")) 1.0 else 0.0
  tau <- if (trust_active)
    lapply(adj, function(nb) rep(tau_init, length(nb)))
  else NULL

  # ---------------------------------------------------------
  # One-off precomputation of the flattened (agent, neighbour) edge list.
  # adj is fixed for the whole simulation, so this is built once here
  # rather than once per round. Used by:
  #  - the vectorised trust update below (whenever trust_active) -- this
  #    is a purely deterministic computation (no RNG involved at all),
  #    so vectorising it changes nothing about the random draws anywhere
  #    in the simulation -- it's a strict speedup with no caveat.
  #  - the delegation-decision fast_path (see that block for the
  #    reproducibility caveat, which does NOT apply to the trust update).
  # ---------------------------------------------------------
  all_lay   <- identical(lay_ids, seq_len(n_all))
  fast_path <- fast_path && cycle_fallback == "none" && all_lay

  # trust_vectorized also requires all_lay: the original trust-update
  # loop below only updates tau[[i]] for i in lay_ids, and the flattened
  # edge list is built over all n_all agents -- only equivalent when
  # every agent is lay (true for every current report; guarded rather
  # than assumed in case that ever changes).
  trust_vectorized <- trust_active && all_lay

  need_edge_flat <- fast_path || trust_vectorized
  if (need_edge_flat) {
    deg     <- lengths(adj)
    edge_i  <- rep(seq_len(n_all), times = deg)
    edge_j  <- unlist(adj, use.names = FALSE)
    n_edges <- length(edge_i)
  }

  if (fast_path) {
    has_nb     <- deg > 0
    edge_split <- if (n_edges > 0) split(seq_len(n_edges), edge_i) else list()
  }

  prev_lost_ids <- integer(0L)   # lay agents whose vote was NA last round
  prev_lost_set <- logical(n_all)
  prev_target   <- integer(n_all) # who each agent delegated to last round (0 = direct)

  # ---------------------------------------------------------
  # Snapshot rounds: historical checkpoints + last 5 rounds
  # ---------------------------------------------------------
  hist_rounds     <- unique(round(c(0.25, 0.50, 0.75, 1.00) * T))
  last5_rounds    <- max(1L, T - 4L):T
  snapshot_rounds <- if (!is.null(snap_rounds)) snap_rounds
                     else sort(unique(c(hist_rounds, last5_rounds)))

  history_lost       <- numeric(T)
  history_drift      <- numeric(T)
  history_delegation <- numeric(T)
  history_stability  <- numeric(T)
  cycle_breakdown_list <- vector("list", T)  # RQ1: per-round (cycle_length -> n_agents) table

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

  # Helper: minimum fraction of agents (by power desc) to reach 50% of total.
  # Denominator is length(x) -- the FULL agent population passed in, zeros
  # (delegators) included -- not length(pv), which would divide only by the
  # count of agents with nonzero power and badly understate concentration.
  share_50pct_fn <- function(x) {
    pv <- sort(x[x > 0], decreasing = TRUE)
    if (!length(pv)) return(NA_real_)
    k <- which(cumsum(pv) / sum(pv) >= 0.5)[1]
    k / length(x)
  }

  for (t in seq_len(T)) {

    pow <- agents$power

    # --------------------------------------------------
    # Delegation decision
    #
    # fast_path (opt-in, see simulate_liquid_democracy() argument doc):
    # computes attractiveness for every (agent, neighbour) edge in the
    # network in one vectorised pass instead of one R-level function
    # call per agent -- the same maths as the loop below, applied
    # elementwise across all ~N*node_degree edges at once. Aggregates
    # per agent (best-neighbour attractiveness, total attractiveness)
    # via tapply() grouped by agent, then does self-vote/delegate-choice
    # sampling. REPRODUCIBILITY NOTE: this draws rnorm()/runif() for all
    # edges/agents up front rather than interleaved per agent, so it
    # consumes the RNG stream in a different order than the loop below --
    # statistically equivalent, not bit-identical for a given seed.
    # --------------------------------------------------
    if (fast_path) {
      op_i_e  <- op[edge_i]
      op_j_e  <- op[edge_j]
      pow_i_e <- pow[edge_i]
      pow_j_e <- pow[edge_j]

      if (sigma_opinion > 0) {
        eps      <- rnorm(n_edges, 0, sigma_opinion)
        op_j_obs <- plogis(qlogis(op_j_e) + eps)
      } else {
        op_j_obs <- op_j_e
      }

      w_e <- attractiveness_fn(op_i_e, op_j_obs, pow_i_e, pow_j_e, r_op, r_pw)

      if (trust_active && gamma != 0) {
        tau_e   <- unlist(tau, use.names = FALSE)  # positionally aligned with edge_i/edge_j
        tau_arg <- if (identical(trust_mode, "relaxation")) tau_e - 1 else tau_e
        w_e     <- w_e * (2 * .sig(gamma * tau_arg))
      }
      if (r_ingroup != 0) {
        # Expose Eq. 10: a_ig = exp(-r_ig*(1-delta)) -- delta=1 (same group)
        # forces a_ig = 1 regardless of r_ig; only cross-group pairs decay.
        delta_g_e <- as.integer(grp[edge_j] == grp[edge_i])
        w_e       <- w_e * exp(-r_ingroup * (1 - delta_g_e))
      }
      w_e <- pmax(w_e, 0)

      # Best-neighbour and total attractiveness per agent, aggregated
      # from the flat edge list (agents with zero neighbours never
      # appear here and correctly stay at the 0 default -- forced to
      # self-vote below via always_self regardless).
      m_all   <- numeric(n_all)
      sum_all <- numeric(n_all)
      if (n_edges > 0) {
        m_by_agent   <- tapply(w_e, edge_i, max)
        sum_by_agent <- tapply(w_e, edge_i, sum)
        idx_agent    <- as.integer(names(m_by_agent))
        m_all[idx_agent]   <- m_by_agent
        sum_all[idx_agent] <- sum_by_agent
      }

      w_self_all  <- p_self_mobius_M(m_all, self_reliance_c, M = 4)
      always_self <- (!has_nb) | (sum_all == 0)
      w_self_all[always_self] <- 1

      votes_self <- always_self | (runif(n_all) < w_self_all)

      targets    <- seq_len(n_all)
      delegating <- which(!votes_self)
      for (i in delegating) {
        idx        <- edge_split[[as.character(i)]]
        targets[i] <- edge_j[idx][sample.int(length(idx), 1L, prob = w_e[idx])]
      }
    } else {
      targets <- vapply(lay_ids, function(i) {
        nb    <- adj[[i]]
        tau_i <- tau[[i]]  # positionally aligned with adj[[i]]; kept in sync with nb below

        # Between-round fallback: if vote was lost last round, adjust choice
        if (cycle_fallback != "none" && prev_lost_set[i]) {
          if (cycle_fallback == "direct") return(i)
          if (cycle_fallback == "redelegate") {
            # exclude the specific delegate that caused the cycle last round
            keep <- nb != prev_target[i]
            nb   <- nb[keep]
            if (trust_active) tau_i <- tau_i[keep]
          } else if (cycle_fallback == "informed") {
            # only consider neighbours whose vote was represented last round
            keep <- !is.na(prev_my_vote[nb])
            nb   <- nb[keep]
            if (trust_active) tau_i <- tau_i[keep]
            if (!length(nb)) return(i)
          }
        }

        # No neighbours → always vote directly
        if (!length(nb)) return(i)

        # ── Endogenous self-weight ────────────────────────────────────────────
        # Compute neighbour attractiveness first (needed for best-neighbour j*)
        op_nb  <- perceive_opinion(op[nb],  sigma_opinion)
        pow_nb <- pow[nb]
        w_nb   <- attractiveness_fn(op[i], op_nb, pow[i], pow_nb, r_op, r_pw)

        if (trust_active && gamma != 0) {
          tau_i_arg <- if (identical(trust_mode, "relaxation")) tau_i - 1 else tau_i
          trust_mod <- 2 * .sig(gamma * tau_i_arg)
          w_nb <- w_nb * trust_mod
        }
        if (r_ingroup != 0) {
          # Expose Eq. 10: a_ig = exp(-r_ig*(1-delta)) -- delta=1 (same group)
          # forces a_ig = 1 regardless of r_ig; only cross-group pairs decay.
          delta_g <- as.integer(grp[nb] == grp[i])
          w_nb    <- w_nb * exp(-r_ingroup * (1 - delta_g))
        }
        w <- pmax(w_nb, 0)

        # If no attractive neighbour exists, vote directly
        if (sum(w) == 0) return(i)

        # Best neighbour j* by attractiveness
        j_idx <- which.max(w)

        # Self-vote probability: Mobius curve through (m=0 -> 1, m=1 -> c,
        # m=M -> 0), M = 4, applied to m = w[j_idx], the best neighbour's
        # attractiveness AFTER trust/ingroup modifiers -- see
        # FunctionVersions.R section (D).
        w_self <- p_self_mobius_M(w[j_idx], self_reliance_c, M = 4)

        if (runif(1) < w_self) return(i)

        nb[sample.int(length(w), 1L, prob = w)]
      }, integer(1L))
    }

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

    # Cycle length breakdown (RQ1): agents with a lost (NA) vote are
    # exactly the ones stuck in a cycle -- cyc_len already computed once by
    # compute_power_and_votes() above (also the source of the corrected
    # cycle-member power, Sec 2.2.7), reused here rather than recomputed.
    # Purely diagnostic, doesn't affect anything above.
    in_cycle_t <- is.na(agents$my_vote)
    if (any(in_cycle_t)) {
      cyc_len_t <- pv$cyc_len
      cycle_breakdown_list[[t]] <- tibble(round = t, cycle_length = cyc_len_t[in_cycle_t]) |>
        dplyr::count(cycle_length, name = "n_agents")
    }

    # --------------------------------------------------
    # Trust update -- Expose Sec 2.2.2: happens only on a VOTING round (see
    # n_voting_rounds/is_voting_round above); frozen on delegating/adaptation
    # rounds. Eq. 4's signal s_ij is defined only for the neighbour i
    # actually delegated to last round (prev_target[i]) -- s_ij = 0 for
    # every other neighbour ("0 if i did not delegate to j"), so under
    # "relaxation" (below) their trust simply drifts back toward the tau=1
    # baseline each voting round ("the trust of agent i towards ALL of its
    # neighbours is slightly restored", Sec 2.2.2), while only the delegate
    # gets the outcome-dependent signal.
    #   "punish_only" (default, Report 17/18 Sec 2.4): tau_ij(t) =
    #     lambda*tau_ij(t-1) - (1-lambda)*|o_i - v_j(t-1)|, only for the
    #     delegate, and only when that delegate's vote was delivered last
    #     round. If the delegate's vote was lost (stuck in a cycle), or i
    #     voted directly, tau is left unchanged for that pair -- this mode
    #     has no baseline-relaxation term.
    #   "reward_punish" (Report 18 Sec 3): tau_ij(t) = lambda*tau_ij(t-1) +
    #     (1-lambda)*s, with s = 1-2|o_i-v_j(t-1)| when the delegate
    #     delivered a vote (positive for agreement, negative for
    #     disagreement -- same 2|.|-1 convention as the attractiveness
    #     formula), s = -cycle_penalty when the delegate's vote was lost,
    #     and s = 0 (tau drifts toward its own baseline 0) for every
    #     neighbour that wasn't the chosen delegate.
    if (is_voting_round[t] && trust_vectorized) {
      if (n_edges > 0) {
        tau_e_old   <- unlist(tau, use.names = FALSE)
        vj_e        <- prev_my_vote[edge_j]
        # Eq. 4: the signal applies only to the neighbour i actually
        # delegated to last round -- everyone else gets s_ij = 0.
        is_target_e <- (prev_target[edge_i] != 0L) & (edge_j == prev_target[edge_i])
        valid_e     <- is_target_e & !is.na(vj_e)  # delegate delivered a vote
        lost_e      <- is_target_e & is.na(vj_e)   # delegate's vote lost to a cycle

        if (trust_mode == "reward_punish") {
          s_e <- numeric(n_edges)
          s_e[valid_e] <- 1 - 2 * abs(op[edge_i[valid_e]] - vj_e[valid_e])
          s_e[lost_e]  <- -cycle_penalty
          tau_e <- lambda * tau_e_old + (1 - lambda) * s_e
        } else if (trust_mode == "relaxation") {
          # tau_ij(t) = (1-lambda)*tau_ij(t-1) + lambda*(1 + s_ij(t-1)),
          # s = k1*(1-2|o_i-v_j|) if the delegate delivered a vote, -k2 if
          # lost to a cycle, 0 otherwise -- lambda here is an ADAPTATION
          # RATE (opposite role from "reward_punish"/"punish_only" above),
          # tau neutral at 1.
          s_e <- numeric(n_edges)
          s_e[valid_e] <- k1 * (1 - 2 * abs(op[edge_i[valid_e]] - vj_e[valid_e]))
          s_e[lost_e]  <- -k2
          tau_e <- (1 - lambda) * tau_e_old + lambda * (1 + s_e)
        } else {
          tau_e <- tau_e_old
          tau_e[valid_e] <- lambda * tau_e_old[valid_e] -
            (1 - lambda) * abs(op[edge_i[valid_e]] - vj_e[valid_e])
        }

        # Regroup the flat per-edge values back into tau[[i]] (one vector
        # per agent, in the same order as adj[[i]]) -- split() is stable
        # (preserves within-group order) and groups come out in ascending
        # agent-id order since edge_i is numeric, so this exactly matches
        # tau's original list-of-vectors shape. Agents with zero
        # neighbours never appear as an edge_i group and are filled back
        # in as empty vectors so tau stays indexed by agent i.
        tau_grouped   <- split(tau_e, edge_i)
        idx_has_edges <- as.integer(names(tau_grouped))
        tau <- vector("list", n_all)
        tau[idx_has_edges] <- tau_grouped
        empty_ids <- setdiff(seq_len(n_all), idx_has_edges)
        for (e in empty_ids) tau[[e]] <- numeric(0)
      }
    } else if (is_voting_round[t] && trust_active) {
      for (i in lay_ids) {
        nb <- adj[[i]]
        if (!length(nb)) next
        vj <- prev_my_vote[nb]
        # Eq. 4: the signal applies only to the neighbour i actually
        # delegated to last round -- everyone else gets s_ij = 0.
        is_target <- (prev_target[i] != 0L) & (nb == prev_target[i])
        valid <- is_target & !is.na(vj)  # delegate delivered a vote
        lost  <- is_target & is.na(vj)   # delegate's vote lost to a cycle

        if (trust_mode == "reward_punish") {
          s          <- numeric(length(nb))
          s[valid]   <- 1 - 2 * abs(op[i] - vj[valid])
          s[lost]    <- -cycle_penalty
          tau[[i]] <- lambda * tau[[i]] + (1 - lambda) * s
        } else if (trust_mode == "relaxation") {
          s          <- numeric(length(nb))
          s[valid]   <- k1 * (1 - 2 * abs(op[i] - vj[valid]))
          s[lost]    <- -k2
          tau[[i]] <- (1 - lambda) * tau[[i]] + lambda * (1 + s)
        } else {
          if (!any(valid)) next
          tau[[i]][valid] <- lambda * tau[[i]][valid] -
            (1 - lambda) * abs(op[i] - vj[valid])
        }
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
      # NA (not 0) when there are no delegators this round -- a genuine
      # delegator's chain length is always >= 1 by construction (shortest
      # distance to a root, excluding roots themselves and cycle members),
      # so 0 used to be a sentinel for "nobody delegated", stored in the
      # same column as real chain lengths. Averaging that 0 in alongside
      # genuine >=1 values (e.g. across a last-5-round window) could pull
      # the reported mean below 1, which is not a real chain length -- just
      # contamination from the sentinel. NA lets callers' mean(., na.rm=TRUE)
      # correctly skip these rounds instead.
      if (length(roots_snap) > 0 && ecount(gD) > 0) {
        dist_mat      <- distances(gD, mode = "out", to = roots_snap)
        chain_lengths <- apply(dist_mat, 1, function(d) {
          fd <- d[is.finite(d)]; if (length(fd)) min(fd) else NA_real_
        })
        delegating_lengths          <- chain_lengths[!is.na(chain_lengths) & chain_lengths > 0]
        avg_chain_length_delegators <- if (length(delegating_lengths) > 0)
          mean(delegating_lengths) else NA_real_
      } else {
        avg_chain_length_delegators <- NA_real_
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

      # ---- Minority metrics ------------------------------------------------
      min_ids_s   <- which(agents$group == "minority")
      maj_ids_s   <- which(agents$group == "majority")
      min_pop_shr <- length(min_ids_s) / n_all

      # Descriptive voting power (P_M, P_Maj, Report 19 item 1) -- power
      # actually cast after votes are resolved. compute_power() gives every
      # agent a baseline power=1 that is NEVER zeroed out for delegators, so
      # summing agents$power over a group double-counts: a delegator's own
      # baseline entry counts once directly, and again inside whichever
      # root's accumulated power they contributed to. Only roots (no
      # outgoing delegation edge) actually cast a vote, so restrict to
      # roots_snap -- a delegator's "final voting power" is correctly 0,
      # not 1, since their vote is cast by their root, not by them.
      root_power <- numeric(n_all)
      if (length(roots_snap) > 0) root_power[roots_snap] <- agents$power[roots_snap]

      # Population-wide power-concentration metrics (gini_power,
      # top5_power_share, share_50pct below) use root_power directly --
      # full n_all length, zero-padded for delegators (see root_power
      # above) -- so "top 5%"/"50% of agents" are taken against the WHOLE
      # population, per the Expose's Table-1 definitions. rep_power used to
      # be `represented$power` (every represented agent, delegators
      # included), which pads the distribution with a spurious "1" per
      # delegator and understates real concentration; a later fix narrowed
      # it to `root_power[roots_snap]` (roots only), which instead shrank
      # the denominator these three metrics divide by down to the number
      # of roots -- silently changing what "5%"/"agents" means and badly
      # UNDERstating concentration whenever most agents delegate (the norm
      # here). Both bugs are fixed by using the full zero-padded vector.
      rep_power <- root_power

      minority_power <- sum(root_power[min_ids_s])
      majority_power <- sum(root_power[maj_ids_s])

      if (min_pop_shr > 0 && sum(root_power) > 0) {
        min_pwr_shr <- minority_power / sum(root_power)
        RR_m        <- min_pwr_shr / min_pop_shr
      } else {
        min_pwr_shr <- 0; RR_m <- NA_real_
      }

      cross_grp_rate <- if (length(edge_from) > 0)
        mean(agents$group[edge_from] != agents$group[edge_to])
      else 0

      # Symmetric, directional cross-group delegation rates (Report 19 item
      # 4): share of each group's OWN outgoing delegation edges that cross
      # into the other group. Replaces the population-wide cross_grp_rate
      # above (kept for backward compatibility with earlier reports) with
      # two group-restricted rates that don't get dominated by whichever
      # group is larger.
      from_is_min <- agents$group[edge_from] == "minority"
      from_is_maj <- agents$group[edge_from] == "majority"
      cdr_min_to_maj <- if (any(from_is_min))
        mean(agents$group[edge_to[from_is_min]] != "minority") else NA_real_
      cdr_maj_to_min <- if (any(from_is_maj))
        mean(agents$group[edge_to[from_is_maj]] == "minority") else NA_real_

      # Group-specific delegation_rate / lost_vote_rate (Report 19): same
      # definitions as the population-wide history_delegation/history_lost,
      # restricted to each group's own agents.
      minority_delegation_rate <- mean(agents$delegated[min_ids_s])
      majority_delegation_rate <- mean(agents$delegated[maj_ids_s])
      minority_lost_vote_rate  <- mean(is.na(agents$my_vote[min_ids_s]))
      majority_lost_vote_rate  <- mean(is.na(agents$my_vote[maj_ids_s]))

      # Number of distinct representatives (roots) contributed by each group
      # -- Report 19's group-specific stand-in for total_components, and the
      # same r_M / r_Maj quantity introduced as "descriptive counts" in
      # Report 15 Section 3.
      minority_n_reps <- if (length(roots_snap) > 0) sum(agents$group[roots_snap] == "minority") else 0L
      majority_n_reps <- if (length(roots_snap) > 0) sum(agents$group[roots_snap] == "majority") else 0L

      # Minority Voter Self-Representation (VSR_m): share of resolved
      # minority agents (by headcount) whose delegation chain root is
      # itself minority. Agents stuck in a cycle have no defined root and
      # are excluded from both numerator and denominator -- matching
      # Minority Power Capture (PC_m)'s resolved-only convention, so the
      # two are directly comparable (headcount vs. power-weighted versions
      # of the same resolved-only question).
      root_group_s   <- ifelse(is.na(pv$roots), NA_character_, agents$group[pv$roots])
      min_repr_ids_s <- min_ids_s[!is.na(root_group_s[min_ids_s])]
      maj_repr_ids_s <- maj_ids_s[!is.na(root_group_s[maj_ids_s])]
      min_self_rep_rate <- if (length(min_repr_ids_s) > 0)
        mean(root_group_s[min_repr_ids_s] == "minority")
      else NA_real_

      # RR_m computed against the RESOLVED population share instead of the
      # total population share (n_min/n_all). RR_m (above) folds two
      # channels into one number: majority capturing minority power, and
      # minority votes lost to cycles (which shrink minority_power but not
      # the fixed population-share denominator). RR_m_resolved isolates just
      # the capture channel by dividing the same resolved-power numerator by
      # the resolved *population* share instead -- directly comparable to
      # PC_m, which is also resolved-only by construction.
      n_resolved_min   <- length(min_repr_ids_s)
      n_resolved_maj   <- length(maj_repr_ids_s)
      n_resolved_total <- n_resolved_min + n_resolved_maj
      RR_m_resolved <- if (n_resolved_total > 0 && sum(root_power) > 0)
        min_pwr_shr / (n_resolved_min / n_resolved_total)
      else NA_real_

      # Minority Power Capture (PC_m, Report 19 item 3): fraction of the
      # minority's total voting power ultimately represented by a majority
      # delegate. NOTE: every agent contributes exactly 1 unit of power to
      # wherever their chain terminates (compute_power() has no heterogeneous
      # weighting), so under the current model this is numerically identical
      # to 1 - minority_self_rep_rate -- kept as its own named column per
      # Report 15's definition, and would diverge from the headcount version
      # if the model is ever extended with non-uniform starting power.
      minority_capture <- if (length(min_repr_ids_s) > 0)
        mean(root_group_s[min_repr_ids_s] == "majority")
      else NA_real_

      # Majority Power Capture (PC_Maj, symmetric counterpart requested for
      # Report 19): fraction of the majority's resolved agents ultimately
      # represented by a MINORITY delegate -- same headcount-equals-power
      # caveat as PC_m above.
      majority_capture <- if (length(maj_repr_ids_s) > 0)
        mean(root_group_s[maj_repr_ids_s] == "minority")
      else NA_real_

      min_vtr <- min_ids_s[!is.na(agents$my_vote[min_ids_s])]
      # Same root-restriction fix as rep_power above -- min_enp used to
      # include every represented minority agent's power (agents$power[min_vtr]),
      # padding the distribution with delegators' spurious baseline-1 power
      # and understating true concentration among the minority's own roots.
      min_root_ids_s <- intersect(min_ids_s, roots_snap)
      min_enp <- if (length(min_root_ids_s) > 0) {
        pm <- root_power[min_root_ids_s]
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

      # Majority-side mirrors of min_chain/min_drift (Report 19: chain
      # length and drift, group-specific).
      maj_vtr <- maj_ids_s[!is.na(agents$my_vote[maj_ids_s])]
      maj_dlg <- maj_ids_s[agents$delegated[maj_ids_s]]
      maj_chain <- if (length(maj_dlg) > 0 && length(roots_snap) > 0 && ecount(gD) > 0) {
        dm  <- distances(gD, v = maj_dlg, to = roots_snap, mode = "out")
        cls <- apply(dm, 1, function(d) { fd <- d[is.finite(d)]; if (length(fd)) min(fd) else NA_real_ })
        mean(cls, na.rm = TRUE)
      } else 0

      maj_rep   <- agents[maj_vtr, ]
      maj_drift <- if (nrow(maj_rep) > 0)
        mean(abs(maj_rep$opinion - maj_rep$my_vote)) else NA_real_

      snapshot_list[[as.character(t)]] <- tibble(
        round                       = t,
        pct_T                       = t / T,
        lost_vote_rate              = history_lost[t],
        avg_drift                   = history_drift[t],
        delegation_rate             = history_delegation[t],
        gini_power                  = gini(rep_power),
        top5_power_share            = top5(rep_power),
        share_50pct                 = share_50pct_fn(rep_power),
        avg_chain_length_delegators = avg_chain_length_delegators,
        largest_voting_bloc_share   = largest_voting_bloc,
        total_components            = total_components,
        delegation_stability        = stab,
        flickerness                 = stab,  # F(t) = 1(d_i(t)=d_i(t-1)) averaged over i;
                                              # NA on round 1 (no prior decision to compare
                                              # against). Delegation decisions are made every
                                              # round (voting and delegating alike, Sec 1.4.1),
                                              # so no gating on is_voting_round is needed here --
                                              # same value as delegation_stability, kept under
                                              # both names for callers expecting either.
        is_voting_round             = is_voting_round[t],
        minority_power              = minority_power,
        majority_power              = majority_power,
        minority_power_share        = min_pwr_shr,
        RR_m                        = RR_m,
        RR_m_resolved               = RR_m_resolved,
        minority_n_resolved         = n_resolved_min,
        majority_n_resolved         = n_resolved_maj,
        minority_capture            = minority_capture,
        majority_capture            = majority_capture,
        minority_self_rep_rate      = min_self_rep_rate,
        cross_group_dlg_rate        = cross_grp_rate,
        cdr_min_to_maj              = cdr_min_to_maj,
        cdr_maj_to_min              = cdr_maj_to_min,
        minority_delegation_rate    = minority_delegation_rate,
        majority_delegation_rate    = majority_delegation_rate,
        minority_lost_vote_rate     = minority_lost_vote_rate,
        majority_lost_vote_rate     = majority_lost_vote_rate,
        minority_n_reps             = minority_n_reps,
        majority_n_reps             = majority_n_reps,
        minority_enp                = min_enp,
        minority_chain_len          = min_chain,
        minority_drift              = min_drift,
        majority_chain_len          = maj_chain,
        majority_drift              = maj_drift,
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
    agents               = agents,
    history_lost         = history_lost,
    history_drift        = history_drift,
    history_delegation   = history_delegation,
    history_stability    = history_stability,
    history_flickerness  = history_stability,  # alias -- see snapshot_list's "flickerness" column
    voting_rounds        = voting_rounds,      # which of the T rounds had a trust update (Sec 1.4.1)
    cycle_breakdown      = bind_rows(cycle_breakdown_list),  # tibble: round, cycle_length, n_agents
    snapshots            = bind_rows(snapshot_list),
    delegation_graphs    = delegation_graphs,
    final_graph          = delegation_graphs[[T]],
    friendship_graph     = gF,
    tau_final            = tau,  # final trust state (list, one vector per agent, positionally aligned
                                  # with neighbors(gF, v, mode="out")); NULL if trust was never active
                                  # (lambda==0 && gamma==0). Output-only addition -- does not affect any
                                  # model equation, purely exposes existing internal state for
                                  # post-hoc attractiveness recomputation (Report_22 Sec 1).
    sim_time_sec         = as.numeric(difftime(Sys.time(), sim_time_start, units = "secs"))
  )
}
