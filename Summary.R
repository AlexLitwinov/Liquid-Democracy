summary_metrics <- function(res) {
  library(igraph)
  library(dplyr)
  
  agents <- res$agents
  gD <- res$delegation_graph
  
  # ---------------------------------------------------------
  # 1. NETWORK DESCRIPTION
  # ---------------------------------------------------------
  
  comps <- components(gD)
  
  gini <- function(x) {
    if (sum(x) == 0) return(0)
    x <- sort(x); n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }
  
  # Represented agents 
  represented_agents <- agents %>% filter(!is.na(my_vote))
  
  # --- Delegation Chain Length ---
  get_chain_length <- function(node, gD) {
    chain_len <- 0
    current <- node
    visited <- c()
    
    repeat {
      next_node <- neighbors(gD, current, mode = "out")
      
      if (length(next_node) == 0) break
      if (as.numeric(next_node) %in% visited) return(NA)  # cycle
      
      visited <- c(visited, current)
      current <- as.numeric(next_node)
      chain_len <- chain_len + 1
    }
    
    return(chain_len)
  }
  
  if (nrow(represented_agents) > 0) {
    chain_lengths <- sapply(represented_agents$id, get_chain_length, gD = gD)
    avg_chain_length <- mean(chain_lengths, na.rm = TRUE)
  } else {
    avg_chain_length <- NA
  }
  
  network_description <- list(
    vote_attribution_rate    = 1 - mean(is.na(agents$my_vote)),
    lost_vote_rate           = mean(is.na(agents$my_vote)),
    mean_power               = mean(agents$countdelegations),
    max_power                = max(agents$countdelegations),
    gini_power_inequality    = gini(agents$countdelegations),
    avg_chain_length         = avg_chain_length,
    total_components         = comps$no,
    largest_voting_bloc_size = max(comps$csize)
  )
  
  # ---------------------------------------------------------
  # 2. DYNAMIC EVALUATION
  # ---------------------------------------------------------
  
  avg_ideological_drift <- if(nrow(represented_agents) > 0) {
    mean(abs(represented_agents$preference - represented_agents$my_vote))
  } else { NA }
  
  # Direct democracy votes
  direct_votes <- ifelse(agents$preference >= 0.5, 1, 0)
  direct_yes  <- sum(direct_votes == 1)
  direct_no   <- sum(direct_votes == 0)
  
  # Liquid democracy votes
  liquid_votes <- ifelse(represented_agents$my_vote >= 0.5, 1, 0)
  liquid_yes <- sum(liquid_votes == 1, na.rm = TRUE)
  liquid_no  <- sum(liquid_votes == 0, na.rm = TRUE)
  
  majority_shift <- (liquid_yes - liquid_no) - (direct_yes - direct_no)
  
  dynamic_evaluation <- list(
    avg_ideological_drift = avg_ideological_drift,
    direct_yes            = direct_yes,
    direct_no             = direct_no,
    liquid_yes            = liquid_yes,
    liquid_no             = liquid_no,
    direct_liquid_shift   = majority_shift
  )
  
  # ---------------------------------------------------------
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