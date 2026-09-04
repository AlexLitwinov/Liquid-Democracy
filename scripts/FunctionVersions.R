# =============================================================
# FUNCTION VERSION LIBRARY
#
# Contains all versioned formulas for:
#   (A) Neighbour attractiveness  — passed as attractiveness_fn
#   (B) Self-weight               — passed as selfweight_fn
#
# Usage:
#   simulate_liquid_democracy(...,
#     r_op = 2, r_pw = 2   # uses dual-sigmoid defaults
#   )
#   # or override with an archived formula:
#   simulate_liquid_democracy(...,
#     r_op = 2, r_pw = 2,
#     attractiveness_fn = attractiveness_log
#   )
#
# API (all versions):
#   attractiveness_fn(op_i, op_j, pow_i, pow_j, r_op, r_pw)
#   selfweight_fn(i, nb, op, pow, r_op, r_pw)
#
# Old versions are preserved below and must never be deleted.
# =============================================================

.sig <- function(x) 1 / (1 + exp(-x))


# =============================================================
# (A) NEIGHBOUR ATTRACTIVENESS FORMULAS
#
# Interface (identical across all versions):
#   op_i, op_j   — opinions of agent i and j  in [0, 1]
#   pow_i, pow_j — current power of agent i and j  (>= 1)
#   r_op         — opinion sensitivity (sigmoid steepness)
#   r_pw         — power sensitivity   (sigmoid steepness)
#
# Returns: scalar attractiveness weight A_ij in (0, 1)
# =============================================================

# Attractiveness v1 — Linear Opinion × Absolute Power Difference  [archived]
# A_ij = (1 - |op_i - op_j|) * σ(r_pw * (pow_j - pow_i))
#
# Linear opinion proximity. Power term uses raw difference — saturates
# once a power hierarchy forms, so r_pw loses meaning over rounds.
attractiveness_absolute <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  (1 - abs(op_i - op_j)) * .sig(r_pw * (pow_j - pow_i))
}


# Attractiveness v2 — Linear Opinion × Log-Ratio Power  [archived]
# A_ij = (1 - |op_i - op_j|) * σ(r_pw * log(pow_j / pow_i))
#
# Replaces absolute power difference with the log ratio: a 5:1 ratio
# gives the same sigmoid input regardless of absolute scale, so r_pw
# retains a consistent interpretation. Opinion is still linear.
attractiveness_log <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  (1 - abs(op_i - op_j)) * .sig(r_pw * log(pow_j / pow_i))
}


# Attractiveness v3 — Dual Sigmoid  [current default]
# A_ij = σ(r_op * (1 - 2|op_i - op_j|)) * σ(r_pw * log(pow_j / pow_i))
#
# Both opinion proximity and power ratio are wrapped in independent
# sigmoids. r_op and r_pw separately control how sharply each dimension
# influences delegation. Similarity (1 - 2|Δop|) maps to (-1, 1):
#   Δop = 0   → s = +1 → σ(r_op) > 0.5  (attractive)
#   Δop = 0.5 → s =  0 → σ(0) = 0.5     (neutral)
#   Δop = 1   → s = -1 → σ(-r_op) < 0.5 (repulsive)
attractiveness_dual_sigmoid <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  .sig(r_op * (1 - 2 * abs(op_i - op_j))) * .sig(r_pw * log(pow_j / pow_i))
}


# Attractiveness v4 — Gaussian Opinion x Logistic Log-Power
#   [decision_model = "gaussian_mobius", Report 20]
# A_ij = 2 * a_op_ij * a_pw_ij
#   a_op_ij = exp(-r_op^2 * (op_i - op_j)^2 / 2)  — Gaussian kernel in
#             opinion distance: = 1 at zero distance, -> 0 far away.
#             Width sigma = 1/r_op (larger r_op = sharper opinion gating).
#   a_pw_ij = σ(r_pw * log(pow_j / pow_i))        — = 0.5 at equal power,
#             same log-ratio sigmoid as v2/v3.
#
# Unlike attractiveness_dual_sigmoid, opinion proximity is a genuine
# Gaussian (not squashed through a sigmoid). The leading "2 *" matches
# the scale of the existing trust_mod = 2*sig(gamma*tau) multiplier
# (Network.R): with trust/ingroup inactive (gamma = 0, r_ingroup = 0),
# an identical neighbour (Δop = 0, equal power) sits at A_ij = 1, and
# the unreachable ceiling is A_ij = 2 — see p_self_mobius() below, which
# is calibrated to that same [0, 2] scale.
attractiveness_gaussian_log <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  a_op <- exp(-0.5 * r_op^2 * (op_i - op_j)^2)
  a_pw <- .sig(r_pw * log(pow_j / pow_i))
  2 * a_op * a_pw
}


# Attractiveness v5 -- Exponential-Absolute Opinion x Own-Scaled Logistic Power
#   [decision_model = "calib", Report 21 calibration pipeline]
# A_ij = a_op_ij * a_pw_ij  (a_tr_ij multiplied in separately by Network.R's
#        existing trust_mod path -- see trust_mode = "relaxation" there; a_ig
#        held at 1 throughout Part I)
#   a_op_ij = exp(-r_op * |op_i - op_j|)          in (0, 1]  -- NOT squared,
#             NOT Gaussian, and carries no leading "2" (unlike v4): decays
#             exponentially in raw opinion distance, width 1/r_op.
#   a_pw_ij = 2 * L(r_pw * log(pow_j / pow_i))    in (0, 2)  -- same log-ratio
#             sigmoid as v3/v4, but now carries its OWN leading "2" (unlike
#             v4, where the "2" sat outside the whole product). This is the
#             "log" formula validated against the linear-ratio alternative
#             in reports/testing_power.Rmd.
#
# At r_op = r_pw = 0: a_op = 1, a_pw = 2*L(0) = 1, so an identical,
# equally-powerful, untrusted neighbour sits at A_ij = 1, matching v4's
# convention -- but the ceiling here is A_ij = 2 * 1 = 2 from a_pw alone
# (a_op's ceiling is only 1), and with a_tr also capable of reaching 2, the
# theoretical maximum of the full product a_tr*a_pw*a_op is now 4, not 2 --
# see p_self_mobius_M() below, calibrated to M = 4.
attractiveness_calib <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  a_op <- exp(-r_op * abs(op_i - op_j))
  a_pw <- 2 * .sig(r_pw * log(pow_j / pow_i))
  a_op * a_pw
}


# Attractiveness v6 -- Exponential-Absolute Opinion x Exact Expose Power-Ratio
#   [decision_model = "expose" -- exact match to Expose Eq. 7 and Eq. 9]
# A_ij = a_op_ij * a_pw_ij  (a_tr_ij and a_ig_ij multiplied in separately by
#        Network.R, exactly as for attractiveness_calib() above)
#   a_op_ij = exp(-r_op * |op_i - op_j|)               -- identical to v5
#   a_pw_ij = 2 * L(r_pw * (pow_j / pow_i - 1))          -- power RATIO MINUS 1,
#             matching Expose Eq. 7 exactly. v5 (attractiveness_calib) uses
#             log(pow_j/pow_i) instead -- kept there, unchanged, so any
#             existing report calling decision_model = "calib" keeps
#             reproducing its cached results. This version is what backs the
#             new decision_model = "expose" (see simulate_liquid_democracy()).
#
# Same ceiling as v5: a_op in (0,1], a_pw in (0,2) -> A_ij in (0,2), and with
# a_tr/a_ig each capable of reaching their own ceiling (2 and 1 respectively),
# the full product a_tr*a_pw*a_op*a_ig has theoretical maximum 4 -- same M=4
# used by p_self_mobius_M() below.
attractiveness_expose <- function(op_i, op_j, pow_i, pow_j, r_op, r_pw) {
  a_op <- exp(-r_op * abs(op_i - op_j))
  a_pw <- 2 * .sig(r_pw * (pow_j / pow_i - 1))
  a_op * a_pw
}


# =============================================================
# (B) SELF-WEIGHT FORMULAS
#
# Interface (identical across all versions):
#   i    — integer index of the focal agent
#   nb   — integer vector of out-neighbour indices
#   op   — numeric vector of all agents' opinions
#   pow  — numeric vector of all agents' current power
#   r_op — opinion sensitivity
#   r_pw — power sensitivity
#
# Returns: scalar w_self in (0, 1)
# =============================================================

# Self-weight v1 — Absolute Own Power  [archived]
# w_self = σ(r_pw * pow_i)
#
# Self-confidence proportional to own absolute power, no social
# comparison. At high r_pw even power = 1 gives w_self ≈ 1,
# suppressing delegation before any hierarchy can form.
selfweight_absolute_power <- function(i, nb, op, pow, r_op, r_pw) {
  .sig(r_pw * pow[i])
}


# Self-weight v2 — Mean Contest  [archived]
# w_self = mean_j σ(r_pw * (pow_i - pow_j))
#
# Averages win-probabilities against all neighbours. Ideology-blind:
# a powerful ideologically distant neighbour suppresses self-weight
# just as much as an ideologically close one.
selfweight_mean_contest <- function(i, nb, op, pow, r_op, r_pw) {
  if (!length(nb)) return(0.5)
  mean(.sig(r_pw * (pow[i] - pow[nb])))
}


# Self-weight v3 — Best-Neighbour Argmax, Absolute  [archived]
# j* = argmax_j [(1 - |op_i - op_j|) * σ(r_pw * (pow_j - pow_i))]
# w_self = σ(r_pw * (pow_i - pow_j*))
#
# Two-step logic using absolute power differences throughout.
# Kept for comparison with the log-ratio version below.
selfweight_argmax <- function(i, nb, op, pow, r_op, r_pw) {
  if (!length(nb)) return(0.5)
  a_nb   <- (1 - abs(op[i] - op[nb])) * .sig(r_pw * (pow[nb] - pow[i]))
  j_star <- nb[which.max(a_nb)]
  .sig(r_pw * (pow[i] - pow[j_star]))
}


# Self-weight v4 — Best-Neighbour Argmax, Log-Ratio  [archived]
# j* = argmax_j [(1 - |op_i - op_j|) * σ(r_pw * log(pow_j / pow_i))]
# w_self = σ(r_pw * log(pow_i / pow_j*))
#
# Same two-step logic as v3, but uses log-ratio power throughout.
# r_pw retains consistent interpretation across all rounds.
selfweight_argmax_log <- function(i, nb, op, pow, r_op, r_pw) {
  if (!length(nb)) return(0.5)
  a_nb   <- (1 - abs(op[i] - op[nb])) * .sig(r_pw * log(pow[nb] / pow[i]))
  j_star <- nb[which.max(a_nb)]
  .sig(r_pw * log(pow[i] / pow[j_star]))
}


# Self-weight v5 — Dual Sigmoid Argmax  [current default]
# j* = argmax_j A_ij  (dual-sigmoid attractiveness)
# w_self = σ(r_op * (2|op_i - op_j*| - 1)) * σ(r_pw * log(pow_i / pow_j*))
#
# Mirror image of attractiveness_dual_sigmoid applied to the best
# neighbour j*. The opinion term flips sign:
#   (2|Δop| - 1) maps to (-1, 1) — negative when close (low self-weight),
#   positive when far (high self-weight).
# Combined with the power term, agent i retains high self-weight when
# its most attractive neighbour is both ideologically distant AND
# more powerful — i.e. when delegation would require a large compromise.
selfweight_dual_sigmoid <- function(i, nb, op, pow, r_op, r_pw) {
  if (!length(nb)) return(0.5)
  a_nb   <- .sig(r_op * (1 - 2 * abs(op[i] - op[nb]))) *
            .sig(r_pw * log(pow[nb] / pow[i]))
  j_star <- nb[which.max(a_nb)]
  .sig(r_op * (2 * abs(op[i] - op[j_star]) - 1)) *
  .sig(r_pw * log(pow[i] / pow[j_star]))
}


# =============================================================
# (C) SELF-VOTE PROBABILITY — Mobius curve
#   [decision_model = "gaussian_mobius", Report 20]
#
# Used together with attractiveness_gaussian_log() above in place of the
# dual-sigmoid self-weight computed inline in Network.R. Unlike the
# selfweight_* formulas in section (B), this formula doesn't need i's or
# j*'s opinion/power directly — only m, the best neighbour's
# ALREADY-COMPUTED attractiveness (after trust and ingroup modifiers, on
# the [0, 2] scale that attractiveness_gaussian_log() and the existing
# trust_mod produce).
#
# Interface:
#   m — attractiveness of agent i's single best neighbour, in [0, 2]
#   c — self-reliance knob, in (0, 1)
#
# Returns: P_self, the probability that i votes directly rather than
# delegating.
#
# P_self(m) = (2 - m) / (2 + m * (1 - 2c) / c)
#   m = 0 (worst possible neighbour)  -> P_self = 1  (always vote directly)
#   m = 1 (identical neighbour)       -> P_self = c  (self-reliance knob)
#   m = 2 (best possible neighbour)   -> P_self = 0  (always delegate)
#
# c is the free self-reliance parameter: large c = self-reliant (delegate
# only to someone clearly better than me), small c = delegation-happy.
# =============================================================
p_self_mobius <- function(m, c) {
  (2 - m) / (2 + m * (1 - 2 * c) / c)
}


# =============================================================
# (D) SELF-VOTE PROBABILITY -- general-ceiling Mobius curve
#   [decision_model = "calib", Report 21 calibration pipeline]
#
# Same Mobius shape as p_self_mobius() above, generalised to an explicit
# ceiling M instead of the hard-coded M = 2. Used with M = 4, matching
# attractiveness_calib()'s theoretical maximum (a_pw's own ceiling of 2,
# times a_tr's own ceiling of 2, times a_op's/a_ig's ceiling of 1 each).
#
# Interface:
#   m — attractiveness of agent i's single best neighbour, in [0, M]
#   c — self-reliance knob, in (0, 1)
#   M — theoretical ceiling of the attractiveness product (default 4)
#
# P_self(m) = (M - m) / (M + m * ((M-1)/c - M))
#   m = 0        -> P_self = 1  (always vote directly)
#   m = 1        -> P_self = c  (self-reliance knob -- m = 1 is where an
#                   identical, untrusted, equally-powerful neighbour sits,
#                   NOT the midpoint M/2, since M > 2 here)
#   m = M        -> P_self = 0  (always delegate)
# =============================================================
p_self_mobius_M <- function(m, c, M = 4) {
  (M - m) / (M + m * ((M - 1) / c - M))
}


# =============================================================
# ARCHIVED NETWORK FEATURES
#
# Removed from Network.R to keep the active model lean.
# Each block is self-contained and documents exactly where
# and how to re-insert the feature.
# =============================================================

# ── perceive_power ────────────────────────────────────────────
# Log-space Gaussian noise on observed neighbour power.
# Parameter: sigma_pow (SD >= 0; 0 = perfect observation).
#
# To re-enable: add sigma_pow = 0 to simulate_liquid_democracy()
# and replace `pow_nb <- pow[nb]` (in both Mode A and Mode B)
# with: pow_nb <- perceive_power(pow[nb], sigma_pow)
perceive_power <- function(true_val, sigma) {
  if (sigma == 0) return(true_val)
  true_val * exp(rnorm(length(true_val), 0, sigma))
}


# ── connect_experts ───────────────────────────────────────────
# Adds directed lay -> expert edges to the friendship graph.
# Parameters:
#   expert_connectedness — fraction of community lay agents
#                          connected to each expert
#
# To re-enable: add n_experts_per_community = 0 and
# expert_connectedness = 0 to simulate_liquid_democracy(), add
# n_experts_per_community to setup_agents() (see below), and
# call after setup_friendship_network():
#   gF <- connect_experts(gF, agents, n_per_community,
#                         expert_connectedness, seed)
connect_experts <- function(gF, agents, n_per_community,
                            expert_connectedness, seed = 1) {
  set.seed(seed)
  lay_ids <- which(agents$type == "lay")
  exp_ids <- which(agents$type == "expert")
  if (length(exp_ids) == 0) return(gF)
  k <- ceiling(expert_connectedness * n_per_community)
  new_edges <- do.call(rbind, lapply(exp_ids, function(e) {
    lay_c  <- lay_ids[agents$community[lay_ids] == agents$community[e]]
    chosen <- sample(lay_c, min(k, length(lay_c)))
    cbind(chosen, e)
  }))
  add_edges(gF, as.vector(t(new_edges)))
}


# ── Expert agents — setup_agents version with experts ─────────
# Replace the current setup_agents() with this version to
# re-enable expert agents.  Also pass n_experts_per_community
# to simulate_liquid_democracy() and call connect_experts()
# after setup_friendship_network() (see above).
# Experts always vote directly; exclude from lay_ids loop.
#
# setup_agents <- function(n_per_community, n_communities,
#                          n_experts_per_community, seed = 1,
#                          minority_share = 0) {
#   set.seed(seed)
#   n_lay <- n_per_community * n_communities
#   n_exp <- n_experts_per_community * n_communities
#   n_all <- n_lay + n_exp
#   n_min   <- round(n_lay * minority_share)
#   grp_lay <- c(rep("minority", n_min), rep("majority", n_lay - n_min))
#   agents <- tibble(
#     id        = 1:n_all,
#     type      = c(rep("lay", n_lay), rep("expert", n_exp)),
#     opinion   = runif(n_all),
#     community = c((0:(n_lay - 1)) %% n_communities,
#                   if (n_exp > 0) (0:(n_exp - 1)) %% n_communities
#                   else integer(0)),
#     group     = c(grp_lay, rep("majority", n_exp)),
#     power     = 1L, my_vote = NA_real_, delegated = FALSE
#   )
#   list(agents = agents, n_lay = n_lay, n_exp = n_exp, n_all = n_all)
# }


# ── p_ingroup — structural homophily in WS rewiring ───────────
# Rewires edges preferentially toward same-group agents.
# Parameter: p_ingroup (log-odds weight; 0 = uniform rewiring).
#
# To re-enable: add p_ingroup = 0 to setup_friendship_network()
# and simulate_liquid_democracy(), then replace the plain
# sample() line in the WS rewiring loop with:
#
#   if (p_ingroup != 0) {
#     same_g <- agents$group[cands] == agents$group[u]
#     wts    <- ifelse(same_g, exp(p_ingroup), 1)
#     edge_list[i, 2] <- sample(cands, 1, prob = wts / sum(wts))
#   } else {
#     edge_list[i, 2] <- sample(cands, 1)
#   }


# ── inertia — delegation persistence across rounds ────────────
# With probability `inertia` an agent skips re-evaluation and
# re-delegates to their previous target.
# Parameter: inertia in [0, 1].
#
# To re-enable: add inertia = 0 to simulate_liquid_democracy().
#
# Mode A (fixed p_self) — insert after the p_self self-vote check,
# before the attractiveness computation:
#
#   if (inertia > 0 && prev_target[i] != 0L && runif(1) < inertia)
#     return(prev_target[i])
#
# Mode B (endogenous self-weight) — insert before final sampling,
# after the w_self < runif(1) self-vote check:
#
#   if (inertia > 0 && prev_target[i] != 0L && runif(1) < inertia)
#     return(prev_target[i])
