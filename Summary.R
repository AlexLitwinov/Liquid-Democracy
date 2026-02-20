# Summary metrics
#
summary_metrics <- function(res) {
  
  library(igraph)
  library(dplyr)
  
  agents <- res$agents
  gD <- res$delegation_graph
  
  # -------------------
  # Lost votes
  # -------------------
  lost_rate <- mean(is.na(agents$my_vote))
  
  # -------------------
  # Power distribution
  # -------------------
  power <- agents$countdelegations
  
  # Gini
  gini <- function(x) {
    if (sum(x) == 0) return(0)
    x <- sort(x)
    n <- length(x)
    sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  }
  
  gini_power <- gini(power)
  
  
  # -------------------
  # Network structure
  # -------------------
  comps <- components(gD)
  n_components <- comps$no
  largest_component <- max(comps$csize)
  
  # Average delegation chain length
  avg_distance <- suppressWarnings(
    mean_distance(gD, directed = TRUE, unconnected = TRUE)
  )
  
  # -------------------
  # Expert vs Lay power
  # -------------------
  power_by_type <- agents |>
    group_by(type) |>
    summarise(
      mean_power = mean(countdelegations),
      max_power = max(countdelegations),
      .groups = "drop"
    )
  # -------------------
  # Return tidy summary
  # -------------------
  list(
    lost_vote_rate = lost_rate,
    gini_power = gini_power,
    max_power = max(power),
    mean_power = mean(power),
    n_components = n_components,
    largest_component_size = largest_component,
    avg_delegation_distance = avg_distance,
    power_by_type = power_by_type
  )
}

