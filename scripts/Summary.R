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
  
  represented_agents <- agents %>% filter(!is.na(my_vote))
  
  # Vectorized chain length via igraph distances()
  # Root nodes = agents with out-degree 0 in the delegation graph
  # Chain length = 0 for direct voters, >= 1 for delegating agents
  roots <- which(degree(gD, mode = "out") == 0)
  
  if (length(roots) > 0 && ecount(gD) > 0) {
    dist_mat      <- distances(gD, mode = "out", to = roots)
    chain_lengths <- apply(dist_mat, 1, function(d) {
      finite_d <- d[is.finite(d)]
      if (length(finite_d)) min(finite_d) else 0
    })
    avg_chain_length <- mean(chain_lengths)
  } else {
    avg_chain_length <- 0
  }
  
  network_description <- list(
    vote_attribution_rate    = 1 - mean(is.na(agents$my_vote)),
    lost_vote_rate           = mean(is.na(agents$my_vote)),
    mean_power               = mean(agents$power),
    max_power                = max(agents$power),
    gini_power_inequality    = gini(agents$power),
    avg_chain_length         = avg_chain_length,
    total_components         = comps$no,
    largest_voting_bloc_size = max(comps$csize)
  )
  
  # ---------------------------------------------------------
  # 2. DYNAMIC EVALUATION
  # ---------------------------------------------------------
  
  avg_ideological_drift <- if (nrow(represented_agents) > 0)
    mean(abs(represented_agents$preference - represented_agents$my_vote))
  else NA
  
  # Direct democracy: every agent votes their own preference
  direct_yes <- sum(agents$preference >= 0.5)
  direct_no  <- sum(agents$preference  < 0.5)
  
  # Liquid democracy: represented agents only
  liquid_yes <- sum(represented_agents$my_vote >= 0.5, na.rm = TRUE)
  liquid_no  <- sum(represented_agents$my_vote  < 0.5, na.rm = TRUE)
  
  # Majority margins — positive = Yes majority, negative = No majority
  # Note: liquid_yes + liquid_no < direct_yes + direct_no when votes
  # are lost to cycles, so margins are not directly commensurable.
  liquid_margin <- liquid_yes - liquid_no
  direct_margin <- direct_yes - direct_no
  
  dynamic_evaluation <- list(
    avg_ideological_drift = avg_ideological_drift,
    direct_yes            = direct_yes,
    direct_no             = direct_no,
    liquid_yes            = liquid_yes,
    liquid_no             = liquid_no,
    liquid_margin         = liquid_margin,
    direct_margin         = direct_margin
  )
  
  # ---------------------------------------------------------
  # 3. TYPE-SPECIFIC ANALYSIS
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
    power_by_type       = power_by_type
  )
}