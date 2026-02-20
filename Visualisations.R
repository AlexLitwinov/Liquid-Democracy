#

### plot 
library(ggraph)
library(tidygraph)
library(RColorBrewer)

plot_delegation_network <- function(res) {
  
  gD <- res$delegation_graph
  agents <- res$agents
  
  g_tbl <- as_tbl_graph(gD) |>
    activate(nodes) |>
    mutate(
      power = agents$countdelegations,
      type = agents$type,
      community = factor(agents$community)
    )
  
  ggraph(g_tbl, layout = "fr") +
    geom_edge_link(
      alpha = 0.15,
      edge_width = 0.4,
      arrow = arrow(length = unit(2, "mm"))
    ) +
    geom_node_point(
      aes(size = power + 1,
          color = type)
    ) +
    scale_color_manual(values = c(
      lay = "skyblue",
      expert = "orange"
    )) +
    scale_size(range = c(2, 10)) +
    theme_void() +
    theme(
      legend.position = "right"
    )
}

library(ggraph)
library(tidygraph)
library(ggplot2)

plot_delegation_network <- function(res) {
  
  gD <- res$delegation_graph
  agents <- res$agents
  
  g_tbl <- as_tbl_graph(gD) |>
    activate(nodes) |>
    mutate(
      power = agents$countdelegations,
      type = agents$type
    )
  
  ggraph(g_tbl, layout = "fr") +
    geom_edge_link(
      alpha = 0.2,
      colour = "grey40",
      arrow = arrow(length = unit(3, "mm")),
      end_cap = circle(2, 'mm')
    ) +
    geom_node_point(
      aes(size = power + 1, color = type)
    ) +
    scale_color_manual(values = c(
      lay = "#4DBBD5",
      expert = "#E64B35"
    )) +
    scale_size(range = c(2, 12)) +
    theme_void() +
    ggtitle("Delegation Network")
}


metrics <- summary_metrics(res)

metrics
metrics$power_by_type

plot_delegation_network(res)
plot_delegation_network(res)

ggplot(res$agents, aes(x = type, y = countdelegations, fill = type)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Power by Agent Type")


ggplot(res$agents, aes(x = countdelegations)) +
  geom_histogram(bins = 20, fill = "steelblue", alpha = 0.8) +
  theme_minimal() +
  ggtitle("Distribution of Delegated Power") +
  xlab("Number of Received Delegations") +
  ylab("Frequency")

dist_mat <- distances(res$delegation_graph)

path_lengths <- dist_mat[is.finite(dist_mat) & dist_mat > 0]

ggplot(data.frame(length = path_lengths), aes(x = length)) +
  geom_histogram(bins = 15, fill = "purple") +
  theme_minimal() +
  ggtitle("Distribution of Delegation Path Lengths")
