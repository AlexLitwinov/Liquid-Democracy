# =============================================================
# DIAGNOSTIC: does the pooled (population-wide) homophily shuffle
# target in shuffle_opinions_homophily() actually leave the minority
# group as homophilously clustered as the majority?
#
# Not part of the main pipeline -- ad hoc check, run once, read the
# printed summary. Uses the exact same setup as run_p2_cell()'s
# "homophily" condition in reports/Report_21.Rmd, but only a modest
# number of seeds (r_ingroup/r_background don't affect the pre-loop
# opinion/group shuffle at all, so sweeping them here would be wasted
# compute for this specific question).
# =============================================================
suppressMessages({
  library(here)
  library(igraph)
  library(dplyr)
})
source(here::here("scripts/Network.R"))

N_SIM              <- 500L
K_SIM              <- 14L
P_REWIRE           <- 0.05
MINORITY_SHARE     <- 0.2
HOMOPHILY_TARGET_R <- 0.8
SIM_T              <- 20L
N_SEEDS            <- 30L

diagnose_one <- function(seed) {
  res <- simulate_liquid_democracy(
    seed = seed, n_per_community = N_SIM, n_communities = 1, node_degree = K_SIM, T = SIM_T,
    r_op = 1, r_pw = 1, p_rewire = P_REWIRE, r_ingroup = 0,
    minority_share = MINORITY_SHARE, opinion_dist = "uniform",
    minority_opinion_mu = 0.25, minority_opinion_min = 0,   minority_opinion_max = 0.5,
    majority_opinion_mu = 0.75, majority_opinion_min = 0.5, majority_opinion_max = 1,
    decision_model = "gaussian_mobius", self_reliance_c = 0.5,
    sigma_opinion = "auto", target_homophily = HOMOPHILY_TARGET_R,
    gamma = 1, lambda = 0.5, trust_mode = "reward_punish",
    cycle_penalty = 1, fast_path = TRUE
  )

  gF      <- res$friendship_graph
  agents  <- res$agents
  lay_ids <- which(agents$type == "lay")
  min_ids <- lay_ids[agents$group[lay_ids] == "minority"]
  maj_ids <- lay_ids[agents$group[lay_ids] == "majority"]

  gF_lay <- induced_subgraph(gF, lay_ids)
  gF_min <- induced_subgraph(gF, min_ids)
  gF_maj <- induced_subgraph(gF, maj_ids)

  tibble(
    seed              = seed,
    n_minority        = length(min_ids),
    n_majority        = length(maj_ids),
    n_min_min_edges   = ecount(gF_min),
    n_maj_maj_edges   = ecount(gF_maj),
    assort_overall    = assortativity(gF_lay, agents$opinion[lay_ids], directed = FALSE),
    assort_minority   = if (ecount(gF_min) >= 2) assortativity(gF_min, agents$opinion[min_ids], directed = FALSE) else NA_real_,
    assort_majority   = if (ecount(gF_maj) >= 2) assortativity(gF_maj, agents$opinion[maj_ids], directed = FALSE) else NA_real_
  )
}

cat(sprintf("Running %d seeds under the 'homophily' condition (target r = %.2f)...\n",
            N_SEEDS, HOMOPHILY_TARGET_R))

out <- purrr::map_dfr(seq_len(N_SEEDS), diagnose_one)

cat("\n--- Per-seed results ---\n")
print(out, n = N_SEEDS)

cat("\n--- Summary across seeds ---\n")
summ <- out |>
  summarise(
    mean_assort_overall  = mean(assort_overall,  na.rm = TRUE),
    mean_assort_minority = mean(assort_minority, na.rm = TRUE),
    mean_assort_majority = mean(assort_majority, na.rm = TRUE),
    sd_assort_minority   = sd(assort_minority,   na.rm = TRUE),
    sd_assort_majority   = sd(assort_majority,   na.rm = TRUE),
    mean_n_min_min_edges = mean(n_min_min_edges),
    mean_n_maj_maj_edges = mean(n_maj_maj_edges),
    n_na_minority        = sum(is.na(assort_minority)),
    gap_maj_minus_min    = mean(assort_majority, na.rm = TRUE) - mean(assort_minority, na.rm = TRUE)
  )
print(summ)

saveRDS(out, here::here("data/diagnose_group_homophily.rds"))
cat("\nSaved per-seed results to data/diagnose_group_homophily.rds\n")
