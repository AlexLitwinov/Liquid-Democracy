library(igraph)
library(dplyr)

summary_metrics <- function(res) {

  agents  <- res$agents
  gD      <- res$final_graph
  n_all   <- nrow(agents)
  lay_ids <- which(agents$type == "lay")
  n_lay   <- length(lay_ids)

  # ---------------------------------------------------------
  # Helper functions
  # ---------------------------------------------------------

  gini <- function(x) {
    if (sum(x) == 0) return(0)
    x <- sort(x); n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }

  top5_power_share <- function(power) {
    n_top <- max(1, floor(0.05 * length(power)))
    sum(sort(power, decreasing = TRUE)[1:n_top]) / sum(power)
  }

  # ---------------------------------------------------------
  # Agent subsets
  # ---------------------------------------------------------

  represented_agents <- agents %>% filter(!is.na(my_vote))
  lay_agents         <- agents %>% filter(type == "lay")

  # ---------------------------------------------------------
  # 1. NETWORK DESCRIPTION
  # ---------------------------------------------------------

  # Active nodes: agents with at least one edge in the delegation graph.
  # Includes delegation roots who receive delegations but excludes
  # isolated direct voters (no edges at all).
  active_nodes <- which(degree(gD, mode = "all") > 0)

  if (length(active_nodes) > 0) {
    gD_active           <- induced_subgraph(gD, active_nodes)
    comps               <- components(gD_active)
    largest_voting_bloc <- max(comps$csize) / n_all
    # Total independent voting groups = delegation tree components
    # + all isolated agents (no edge in delegation graph at all)
    n_isolated        <- sum(!seq_len(n_all) %in% active_nodes)
    total_components  <- comps$no + n_isolated
  } else {
    largest_voting_bloc <- 1 / n_all
    total_components    <- n_all
  }

  # Delegation rate: share of lay agents who delegated in the final round.
  # experts are excluded — they never delegate by design.
  delegation_rate <- mean(lay_agents$delegated)

  # Lost vote rate: fraction of LAY agents whose vote was lost to a cycle.
  # Experts always vote directly and never contribute to vote loss.
  lost_vote_rate <- mean(is.na(agents$my_vote[lay_ids]))

  # Chain length: distance from each agent to their delegation root.
  # Direct voters = 0; delegating agents >= 1; cycle agents = NA.
  roots <- which(degree(gD, mode = "out") == 0)

  if (length(roots) > 0 && ecount(gD) > 0) {
    dist_mat      <- distances(gD, mode = "out", to = roots)
    chain_lengths <- apply(dist_mat, 1, function(d) {
      finite_d <- d[is.finite(d)]
      if (length(finite_d)) min(finite_d) else NA_real_
    })
    avg_chain_length_all        <- mean(chain_lengths, na.rm = TRUE)
    delegating_lengths          <- chain_lengths[!is.na(chain_lengths) & chain_lengths > 0]
    avg_chain_length_delegators <- if (length(delegating_lengths) > 0)
      mean(delegating_lengths) else 0
    max_chain_length            <- if (length(delegating_lengths) > 0)
      max(delegating_lengths) else 0
  } else {
    avg_chain_length_all        <- 0
    avg_chain_length_delegators <- 0
    max_chain_length            <- 0
  }

  # Power concentration — two versions:
  #
  # Nominal: computed over REPRESENTED agents only (those with a valid vote).
  #   Measures inequality within the delegation hierarchy that actually operated.
  #   Cycle agents are excluded because their nominal power (=1) is stranded
  #   and does not influence the outcome.
  #
  # Effective: computed over ALL LAY agents.
  #   Cycle agents enter the distribution with effective power = 0 because
  #   their vote is lost and has no influence on the collective outcome.
  #   Measures system-wide inequality including the effect of vote loss.
  rep_power       <- represented_agents$power
  effective_power <- ifelse(is.na(agents$my_vote[lay_ids]), 0L, agents$power[lay_ids])

  # Normalised max power: fraction of total attributed votes held by the
  # single most powerful agent. Comparable across simulations of different size.
  total_attributed <- sum(!is.na(agents$my_vote))
  max_power_raw    <- max(agents$power)
  max_power_share  <- if (total_attributed > 0) max_power_raw / total_attributed else NA_real_

  # In-degree: number of agents delegating DIRECTLY to each agent (non-transitive).
  max_indegree <- max(degree(gD, mode = "in"))

  network_description <- list(
    lost_vote_rate              = lost_vote_rate,
    delegation_rate             = delegation_rate,
    max_power                   = max_power_raw,
    max_power_share             = max_power_share,
    max_indegree                = max_indegree,
    gini_power_nominal          = gini(rep_power),
    top5_power_share_nominal    = top5_power_share(rep_power),
    gini_power_effective        = gini(effective_power),
    top5_power_share_effective  = top5_power_share(effective_power),
    avg_chain_length_all        = avg_chain_length_all,
    avg_chain_length_delegators = avg_chain_length_delegators,
    max_chain_length            = max_chain_length,
    largest_voting_bloc_share   = largest_voting_bloc,
    total_components            = total_components
  )

  # ---------------------------------------------------------
  # 2. DYNAMIC EVALUATION
  # ---------------------------------------------------------

  # Drift: conditional on successful representation (cycle agents excluded).
  # Caveat: if cycle agents are non-randomly distributed across the preference
  # space this estimate is biased. Report alongside lost_vote_rate.
  if (nrow(represented_agents) > 0) {
    drift_vals            <- abs(represented_agents$opinion -
                                   represented_agents$my_vote)
    avg_ideological_drift <- mean(drift_vals)
    med_ideological_drift <- median(drift_vals)
    max_ideological_drift <- max(drift_vals)
    pct_high_drift        <- mean(drift_vals > 0.3)

    # Binary misclassification rate: fraction of represented agents whose
    # attributed vote (>= 0.5 = Yes) contradicts their own preference side.
    # More outcome-relevant than raw drift because it captures directional error.
    misclassification_rate <- mean(
      (represented_agents$my_vote  >= 0.5) !=
      (represented_agents$opinion >= 0.5)
    )
  } else {
    avg_ideological_drift  <- NA
    med_ideological_drift  <- NA
    max_ideological_drift  <- NA
    pct_high_drift         <- NA
    misclassification_rate <- NA
  }

  # Vote counts
  direct_yes <- sum(agents$opinion >= 0.5)
  direct_no  <- sum(agents$opinion  < 0.5)
  liquid_yes <- sum(represented_agents$my_vote >= 0.5, na.rm = TRUE)
  liquid_no  <- sum(represented_agents$my_vote  < 0.5, na.rm = TRUE)

  # Raw margins (not directly commensurable: different effective electorates)
  liquid_margin <- liquid_yes - liquid_no
  direct_margin <- direct_yes - direct_no

  # Proportional margins: divide by the respective electorate size so that
  # margins are comparable across conditions with different vote loss rates.
  liquid_margin_pct <- if ((liquid_yes + liquid_no) > 0)
    liquid_margin / (liquid_yes + liquid_no) else NA_real_
  direct_margin_pct <- direct_margin / (direct_yes + direct_no)

  # Representational efficiency: share of lay votes that were actually cast.
  efficiency <- (liquid_yes + liquid_no) / n_lay

  # Outcome reversal: uses proportional margins so the comparison is
  # between shares of each system's electorate, not raw vote counts.
  direct_outcome   <- ifelse(direct_margin_pct  > 0, "yes", "no")
  liquid_outcome   <- ifelse(!is.na(liquid_margin_pct) & liquid_margin_pct > 0, "yes", "no")
  outcome_reversal <- !is.na(liquid_margin_pct) && direct_outcome != liquid_outcome

  dynamic_evaluation <- list(
    avg_ideological_drift  = avg_ideological_drift,
    med_ideological_drift  = med_ideological_drift,
    max_ideological_drift  = max_ideological_drift,
    pct_high_drift         = pct_high_drift,
    misclassification_rate = misclassification_rate,
    efficiency             = efficiency,
    direct_yes             = direct_yes,
    direct_no              = direct_no,
    liquid_yes             = liquid_yes,
    liquid_no              = liquid_no,
    liquid_margin          = liquid_margin,
    direct_margin          = direct_margin,
    liquid_margin_pct      = liquid_margin_pct,
    direct_margin_pct      = direct_margin_pct,
    outcome_reversal       = outcome_reversal
  )

  # ---------------------------------------------------------
  # 3. CONVERGENCE
  # ---------------------------------------------------------
  # Three levels:
  #   (1) Outcome stability: SD of lost_vote_rate over the last 20 rounds
  #   (2) Behavioural stability: fraction of lay agents keeping the same
  #       delegation target between consecutive rounds
  #   (3) Structural stability: Jaccard similarity of consecutive delegation graphs
  # ---------------------------------------------------------

  convergence_sd <- if (length(res$history_lost) >= 20)
    sd(tail(res$history_lost, 20))
  else
    sd(res$history_lost)

  n_graphs <- length(res$delegation_graphs)

  delegation_stability  <- NA
  edge_similarity_final <- NA
  convergence_round     <- NA

  if (n_graphs >= 2) {

    # Pre-compute edge label sets once to avoid repeated as_edgelist + paste calls
    edge_labels <- lapply(res$delegation_graphs, function(g) {
      el <- as_edgelist(g, names = FALSE)
      if (nrow(el) == 0) character(0) else paste(el[, 1], el[, 2], sep = "-")
    })

    stability_per_pair <- numeric(n_graphs - 1)
    edge_sim_vec       <- numeric(n_graphs - 1)

    for (t in seq_len(n_graphs - 1)) {

      g_prev <- res$delegation_graphs[[t]]
      g_curr <- res$delegation_graphs[[t + 1]]

      target_prev <- integer(n_all)
      target_curr <- integer(n_all)

      el_prev <- as_edgelist(g_prev, names = FALSE)
      el_curr <- as_edgelist(g_curr, names = FALSE)

      if (nrow(el_prev)) target_prev[el_prev[, 1]] <- el_prev[, 2]
      if (nrow(el_curr)) target_curr[el_curr[, 1]] <- el_curr[, 2]

      stability_per_pair[t] <- mean(target_prev[lay_ids] == target_curr[lay_ids])

      e1 <- edge_labels[[t]]
      e2 <- edge_labels[[t + 1]]
      u  <- length(unique(c(e1, e2)))
      edge_sim_vec[t] <- ifelse(u == 0, 1, length(intersect(e1, e2)) / u)
    }

    delegation_stability  <- mean(tail(stability_per_pair, min(20, length(stability_per_pair))))
    edge_similarity_final <- mean(tail(edge_sim_vec,       min(20, length(edge_sim_vec))))

    threshold <- 0.99
    window    <- 50

    if (length(stability_per_pair) >= window) {
      for (i in seq_len(length(stability_per_pair) - window + 1)) {
        if (all(stability_per_pair[i:(i + window - 1)] >= threshold)) {
          convergence_round <- i
          break
        }
      }
    }
  }

  # Oscillation indicator: low behavioural stability combined with high
  # structural similarity suggests a subset of agents cycling between
  # the same targets while the overall graph barely changes.
  cycle_indicator <- if (!is.na(delegation_stability))
    delegation_stability < 0.99 && edge_similarity_final > 0.95
  else NA

  # Degenerate convergence: flags cases where the system converged but only
  # because delegation collapsed — nearly everyone votes directly.
  converged_to_dd <- if (!is.na(convergence_round)) delegation_rate < 0.05 else NA

  convergence <- list(
    convergence_sd        = convergence_sd,
    delegation_stability  = delegation_stability,
    edge_similarity_final = edge_similarity_final,
    convergence_round     = convergence_round,
    cycle_indicator       = cycle_indicator,
    converged_to_dd       = converged_to_dd
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
