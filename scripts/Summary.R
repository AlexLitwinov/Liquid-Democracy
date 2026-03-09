summary_metrics <- function(res) {
  library(igraph)
  library(dplyr)
  
  agents <- res$agents
  gD     <- res$final_graph
  
  # ---------------------------------------------------------
  # 1. NETWORK DESCRIPTION
  # ---------------------------------------------------------
  
  # Restrict components to agents involved in delegation
  # (excludes isolated direct voters who each form a trivial component)
  active_nodes <- which(degree(gD, mode = "all") > 0)
  gD_active    <- induced_subgraph(gD, active_nodes)
  comps        <- components(gD_active)
  
  gini <- function(x) {
    if (sum(x) == 0) return(0)
    x <- sort(x); n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }
  
  # Top-5% power share: fraction of total votes held by the most powerful 5%
  # of agents. Computed over represented agents only (excludes cycle agents
  # with power = 1 which would otherwise deflate the concentration signal).
  top5_power_share <- function(power) {
    n_top <- max(1, floor(0.05 * length(power)))
    sum(sort(power, decreasing = TRUE)[1:n_top]) / sum(power)
  }
  
  represented_agents <- agents %>% filter(!is.na(my_vote))
  lay_agents         <- agents %>% filter(type == "lay")
  
  # Delegation rate: share of lay agents who delegated in round T.
  # agents$delegated is overwritten each round and reflects the final round.
  # TRUE  = agent passed their vote to another agent (did not vote directly)
  # FALSE = agent voted directly with their own preference
  delegation_rate <- mean(lay_agents$delegated)
  
  # Chain length via vectorized igraph distances()
  # Root nodes = agents with out-degree 0 in the delegation graph
  # Chain length = 0 for direct voters, >= 1 for delegating agents
  roots <- which(degree(gD, mode = "out") == 0)
  
  if (length(roots) > 0 && ecount(gD) > 0) {
    dist_mat      <- distances(gD, mode = "out", to = roots)
    chain_lengths <- apply(dist_mat, 1, function(d) {
      finite_d <- d[is.finite(d)]
      if (length(finite_d)) min(finite_d) else 0
    })
    avg_chain_length_all        <- mean(chain_lengths)
    # Chain length among delegating agents only (excludes direct voters with 0)
    # allows fair comparison across simulations with different delegation rates
    delegating_lengths          <- chain_lengths[chain_lengths > 0]
    avg_chain_length_delegators <- if (length(delegating_lengths) > 0)
      mean(delegating_lengths) else 0
  } else {
    avg_chain_length_all        <- 0
    avg_chain_length_delegators <- 0
  }
  
  # Gini and top5 computed over represented agents only (excludes cycle agents
  # with power = 1 which would deflate both metrics)
  rep_power <- represented_agents$power
  
  network_description <- list(
    vote_attribution_rate       = 1 - mean(is.na(agents$my_vote)),
    lost_vote_rate              = mean(is.na(agents$my_vote)),
    delegation_rate             = delegation_rate,
    max_power                   = max(agents$power),
    gini_power_inequality       = gini(rep_power),
    top5_power_share            = top5_power_share(rep_power),
    avg_chain_length_all        = avg_chain_length_all,
    avg_chain_length_delegators = avg_chain_length_delegators,
    largest_voting_bloc_share   = max(comps$csize) / nrow(agents),
    total_components            = comps$no
  )
  
  # ---------------------------------------------------------
  # 2. DYNAMIC EVALUATION
  # ---------------------------------------------------------
  
  # All drift metrics computed over represented agents only (excludes cycles).
  # Caveat: if cycle agents are non-randomly distributed across the preference
  # space, drift estimates may be biased.
  if (nrow(represented_agents) > 0) {
    drift_vals            <- abs(represented_agents$preference -
                                   represented_agents$my_vote)
    avg_ideological_drift <- mean(drift_vals)
    med_ideological_drift <- median(drift_vals)
    max_ideological_drift <- max(drift_vals)
    pct_high_drift        <- mean(drift_vals > 0.3)
  } else {
    avg_ideological_drift <- NA
    med_ideological_drift <- NA
    max_ideological_drift <- NA
    pct_high_drift        <- NA
  }
  
  # Direct democracy: every agent votes their own preference (all agents)
  direct_yes <- sum(agents$preference >= 0.5)
  direct_no  <- sum(agents$preference  < 0.5)
  
  # Liquid democracy: represented agents only
  liquid_yes <- sum(represented_agents$my_vote >= 0.5, na.rm = TRUE)
  liquid_no  <- sum(represented_agents$my_vote  < 0.5, na.rm = TRUE)
  
  # Majority margins: positive = Yes majority, negative = No majority.
  # Note: liquid_yes + liquid_no < direct_yes + direct_no when votes are
  # lost to cycles, so margins are not directly commensurable.
  liquid_margin <- liquid_yes - liquid_no
  direct_margin <- direct_yes - direct_no
  
  # Outcome reversal: does Liquid Democracy flip the majority outcome
  # relative to Direct Democracy? (TRUE = reversal occurred)
  direct_outcome   <- ifelse(direct_margin > 0, "yes", "no")
  liquid_outcome   <- ifelse(liquid_margin > 0, "yes", "no")
  outcome_reversal <- direct_outcome != liquid_outcome
  
  dynamic_evaluation <- list(
    avg_ideological_drift = avg_ideological_drift,
    med_ideological_drift = med_ideological_drift,
    max_ideological_drift = max_ideological_drift,
    pct_high_drift        = pct_high_drift,
    direct_yes            = direct_yes,
    direct_no             = direct_no,
    liquid_yes            = liquid_yes,
    liquid_no             = liquid_no,
    liquid_margin         = liquid_margin,
    direct_margin         = direct_margin,
    outcome_reversal      = outcome_reversal
  )
  
  # ---------------------------------------------------------
  # 3. CONVERGENCE
  # ---------------------------------------------------------
  # Two complementary stability measures:
  #
  # (a) convergence_sd: SD of lost_vote_rate over the last 20 rounds.
  #     Measures whether the fraction of lost votes has stabilised.
  #     Low SD = stable lost vote rate, but does NOT imply stable
  #     delegation structure (two very different structures can produce
  #     the same lost vote rate).
  #
  # (b) delegation_stability: average fraction of lay agents who kept
  #     the SAME delegation target between consecutive rounds, averaged
  #     over the last 20 round-pairs.
  #     0.0 = everyone switches every round (maximally unstable)
  #     1.0 = nobody switches (fully converged)
  #     This directly measures how much the delegation structure is
  #     still changing, independent of vote loss.
  # ---------------------------------------------------------
  
  convergence_sd <- if (length(res$history_lost) >= 20)
    sd(tail(res$history_lost, 20))
  else
    sd(res$history_lost)
  
  n_graphs <- length(res$delegation_graphs)
  n_all    <- nrow(agents)
  lay_ids  <- which(agents$type == "lay")
  
  if (n_graphs >= 2) {
    # Compare last 20 consecutive round-pairs (or fewer if T < 21)
    start_idx <- max(1, n_graphs - 20)
    pairs     <- seq(start_idx, n_graphs - 1)
    
    stability_per_pair <- sapply(pairs, function(t) {
      g_prev <- res$delegation_graphs[[t]]
      g_curr <- res$delegation_graphs[[t + 1]]
      
      # For each agent: who did they delegate to? (0 = voted directly)
      target_prev <- integer(n_all)
      target_curr <- integer(n_all)
      
      el_prev <- as_edgelist(g_prev, names = FALSE)
      el_curr <- as_edgelist(g_curr, names = FALSE)
      
      if (nrow(el_prev)) target_prev[el_prev[, 1]] <- el_prev[, 2]
      if (nrow(el_curr)) target_curr[el_curr[, 1]] <- el_curr[, 2]
      
      # Fraction of lay agents whose target did NOT change
      mean(target_prev[lay_ids] == target_curr[lay_ids])
    })
    
    delegation_stability <- mean(stability_per_pair)
  } else {
    delegation_stability <- NA
  }
  
  convergence <- list(
    convergence_sd       = convergence_sd,
    delegation_stability = delegation_stability
  )
  
  # ---------------------------------------------------------
  # 4. TYPE-SPECIFIC ANALYSIS
  # ---------------------------------------------------------
  
  power_by_type <- agents %>%
    group_by(type) %>%
    summarise(
      mean_power = mean(power),
      max_power  = max(power),
      .groups    = "drop"
    )
  
  list(
    network_description = network_description,
    dynamic_evaluation  = dynamic_evaluation,
    convergence         = convergence,
    power_by_type       = power_by_type
  )
}
