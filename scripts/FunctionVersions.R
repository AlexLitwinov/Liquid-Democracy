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
