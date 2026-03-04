# -----------------------------
# Example run 
# -----------------------------
res <- simulate_liquid_democracy(
  seed = 1,
  n_per_community = 50,
  n_communities = 5,
  node_degree = 6,
  n_experts_per_community = 2,
  expert_connectedness = 0.2,
  p_rewire = 0.5,
  responsiveness = 3,
  T = 500 # number of delegation updates
)

####################
# Summary Function #
####################
stats <- summary_metrics(res)

# structural description
print(stats$network_description)

# dynamic evaluation 
print(stats$dynamic_evaluation)

# table for Expert vs Layperson
print(stats$power_by_type)

#################
# Visualisation #
#################

# Network visualisation
plot_social_and_delegation(res)

# Summary table
get_summary_table(res)

# Summary Visualisation
plot_summary_stats(res)


