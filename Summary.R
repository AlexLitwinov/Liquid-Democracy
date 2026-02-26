summary_metrics <- function(res) {
  library(igraph)
  library(dplyr)
  
  agents <- res$agents
  gD <- res$delegation_graph
  
  # 1. NETWORK DESCRIPTION 
  # ---------------------------------------------------------
  comps <- components(gD)
  
  gini <- function(x) {
    if (sum(x) == 0) return(0)
    x <- sort(x); n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }
  
  network_description <- list(
    vote_attribution_rate    = 1 - mean(is.na(agents$my_vote)),
    lost_vote_rate           = mean(is.na(agents$my_vote)),
    gini_power_inequality    = gini(agents$countdelegations),
    max_power                = max(agents$countdelegations), 
    avg_chain_length         = suppressWarnings(mean_distance(gD, directed = TRUE, unconnected = TRUE)),
    total_components         = comps$no,
    largest_voting_bloc_size = max(comps$csize)
  )
  
  # 2. DYNAMIC EVALUATION 
  # ---------------------------------------------------------
  represented_agents <- agents %>% filter(!is.na(my_vote))
  
  avg_ideological_drift <- if(nrow(represented_agents) > 0) {
    mean(abs(represented_agents$preference - represented_agents$my_vote))
  } else { NA }
  
  systemic_bias <- mean(represented_agents$my_vote, na.rm = TRUE) - mean(agents$preference)
  
  dynamic_evaluation <- list(
    avg_ideological_drift = avg_ideological_drift,
    systemic_bias         = systemic_bias
  )
  
  # 3. TYPE-SPECIFIC ANALYSIS
  # ---------------------------------------------------------
  power_by_type <- agents %>%
    group_by(type) %>%
    summarise(
      mean_power = mean(countdelegations),
      max_power  = max(countdelegations),
      .groups = "drop"
    )
  
  list(
    network_description = network_description,
    dynamic_evaluation  = dynamic_evaluation,
    power_by_type       = power_by_type
  )
}