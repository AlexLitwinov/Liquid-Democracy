# =========================================================
# LIQUID DEMOCRACY SIMULATION: VISUALIZATION & ANALYSIS TOOLKIT
# =========================================================
# Required Libraries
library(ggraph)
library(tidygraph)
library(ggplot2)
library(dplyr)
library(patchwork) 
library(igraph)
library(knitr)

# ---------------------------------------------------------
# 1. NETWORK VISUALIZATION
# ---------------------------------------------------------
# Function: plot_social_and_delegation
# Purpose: Visualizes the underlying social network (friendship) 
# and the resulting delegation paths (active votes).
# ---------------------------------------------------------
plot_social_and_delegation <- function(res) {
  
  # Extract graphs and harmonize edge lists
  g_social <- res$friendship_graph
  g_deleg  <- res$delegation_graph
  
  # Standardize edge data to avoid rbind column mismatches
  edges_social <- igraph::as_data_frame(g_social, what = "edges")[, c("from", "to")]
  edges_social$type_edge <- "social"
  
  edges_deleg <- igraph::as_data_frame(g_deleg, what = "edges")[, c("from", "to")]
  edges_deleg$type_edge <- "delegation"
  
  all_edges <- rbind(edges_social, edges_deleg)
  
  # Create a tidygraph object for advanced styling
  g_combined <- tbl_graph(nodes = res$agents, edges = all_edges, directed = TRUE) %>%
    mutate(
      community_factor = factor(community),
      agent_role = factor(type),
      # Identify final voters (sinks in the delegation graph)
      is_final_voter = degree(g_deleg, mode = "out") == 0,
      label_text = ifelse(is_final_voter, as.character(countdelegations), "")
    )
  
  # Generate Plot
  p <- ggraph(g_combined, layout = "fr") + 
    
    # Layer 1: Underlying Social Structure (Static)
    geom_edge_link(
      aes(filter = (type_edge == "social")),
      color = "grey85", alpha = 0.4, width = 0.3
    ) +
    
    # Layer 2: Active Delegation Flow (Dynamic)
    geom_edge_link(
      aes(filter = (type_edge == "delegation")),
      color = "grey30", alpha = 0.7, width = 0.7,
      arrow = arrow(length = unit(2, "mm")),
      end_cap = circle(3, 'mm')
    ) +
    
    # Layer 3: Agents (Color = Community membership | Shape = Systemic role)
    geom_node_point(
      aes(color = community_factor, shape = agent_role), 
      size = 6,      
      alpha = 0.9
    ) +
    
    # Layer 4: Numeric Labels (Total voting power of final representatives)
    geom_node_text(
      aes(label = label_text),
      color = "white",
      size = 3,     
      fontface = "bold"
    ) +
    
    # Scales and Aesthetics
    scale_color_brewer(palette = "Set1", name = "Community") +
    scale_shape_manual(values = c(expert = 17, lay = 16), name = "Role") +
    theme_graph() +
    labs(
      title = "Liquid Democracy: Clustered by Community & Role",
      subtitle = "Color: Community Membership | Shape: Expert (Triangle) vs. Lay (Circle)",
      caption = "Background: Social Network | Arrows: Active Delegation Flow"
    )
  
  return(p)
}

# ---------------------------------------------------------
# 2. STATISTICAL DIAGRAMS (Summary Metrics)
# ---------------------------------------------------------
# Function: plot_summary_stats
# Purpose: Compares ideological representation (drift) for 
# laypersons and analyzes power concentration across all roles.
# ---------------------------------------------------------
plot_summary_stats <- function(res) {
  
  # A. Analysis of Representation Quality (Laypersons only)
  # Experts are excluded from drift as they always vote their own preference.
  lay_drift_df <- res$agents %>%
    filter(type == "lay") %>%
    filter(!is.na(my_vote)) %>%
    mutate(drift = abs(preference - my_vote))
  
  p1 <- ggplot(lay_drift_df, aes(x = drift)) +
    geom_histogram(
      binwidth = 0.05, fill = "#4DBBD5FF", color = "white", alpha = 0.8
    ) +
    theme_minimal() +
    labs(
      title = "Ideological Representation Loss (Laypersons)",
      subtitle = "Distance between agent preference and cast vote via delegation",
      x = "Drift (0 = Perfect Representation)",
      y = "Count (Laypersons)"
    )
  
  # B. Analysis of Power Distribution (Comparison of Roles)
  power_df <- res$agents %>%
    arrange(desc(countdelegations)) %>%
    mutate(rank = row_number())
  
  p2 <- ggplot(power_df, aes(x = rank, y = countdelegations, fill = type)) +
    geom_col(width = 0.8) + 
    scale_fill_manual(
      values = c(lay = "#4DBBD5FF", expert = "#E64B35FF"),
      labels = c(lay = "Layperson", expert = "Expert")
    ) +
    theme_minimal() +
    labs(
      title = "Influence Ranking & Power Concentration",
      subtitle = "Identification of 'Super-Voters' and Experts' impact",
      x = "Agent Rank (by Power)",
      y = "Votes Collected",
      fill = "Agent Role"
    ) +
    theme(legend.position = "right")
  
  # Combine plots vertically
  return(p1 / p2)
}

# ---------------------------------------------------------
# 3. QUANTITATIVE REPORT (Summary Table)
# ---------------------------------------------------------
# Function: get_summary_table
# Purpose: Aggregates key performance indicators for the 
# simulation into a structured data frame.
# ---------------------------------------------------------
get_summary_table <- function(res) {
  
  agents <- res$agents
  
  # Calculate drift for agents whose votes reached a representative
  drift_data <- agents %>%
    filter(!is.na(my_vote)) %>%
    mutate(drift = abs(preference - my_vote))
  
  # Define Metrics
  summary_stats <- data.frame(
    Category = c(
      "Representation", "Representation", 
      "Inequality", "Inequality", 
      "Opinion Dynamics", "Opinion Dynamics",
      "Network Macro"
    ),
    Metric = c(
      "Lost Vote Rate (%)", "Avg. Delegation Distance",
      "Gini (Power)", "Max Power (Single Agent)",
      "Avg. Ideological Drift", "Max. Ideological Drift",
      "Active Components"
    ),
    Value = c(
      round(mean(is.na(agents$my_vote)) * 100, 2),
      round(mean(distances(res$delegation_graph)[distances(res$delegation_graph) > 0 & is.finite(distances(res$delegation_graph))]), 2),
      round(summary_metrics(res)$network_description$gini_power_inequality, 3),
      max(agents$countdelegations),
      round(mean(drift_data$drift), 3),
      round(max(drift_data$drift), 3),
      count_components(res$delegation_graph)
    )
  )
  
  return(summary_stats)
}