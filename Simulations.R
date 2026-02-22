# -----------------------------
# Example run (like NetLogo: setup + go)
# -----------------------------
res <- simulate_liquid_democracy(
  seed = 123,
  number_of_nodes_per_community = 20,
  number_of_communities = 5,
  node_degree = 6,
  number_of_experts_per_community = 2,
  expert_connectedness = 0.2,
  MS_rewiring_iterations = 50,
  responsiveness_to_power = 1,
  T = 200 # number of delegation updates
)

# Summary Function
stats <- summary_metrics(res)

# structural description
print(stats$network_description)

# dynamic evaluation 
print(stats$dynamic_evaluation)

# table for Expert vs Layperson
print(stats$power_by_type)

