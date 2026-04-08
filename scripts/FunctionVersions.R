# =============================================================
# FUNCTION VERSION LIBRARY
#
# Contains all versioned formulas for:
#   (A) Neighbour attractiveness  — passed as attractiveness_fn
#   (B) Self-weight               — passed as selfweight_fn
#
# Usage:
#   simulate_liquid_democracy(...,
#     attractiveness_fn = attractiveness_log,   # active neighbour formula
#     selfweight_fn     = selfweight_argmax_log  # active self-weight formula
#   )
#
# To switch formulas, replace the function argument. Old versions are
# preserved below and must never be deleted.
# =============================================================


# =============================================================
# (A) NEIGHBOUR ATTRACTIVENESS FORMULAS
#
# Interface (identical across all versions):
#   pref_i, pref_j - ideological preferences of agent i and j
#   pow_i,  pow_j  - current power of agent i and j
#   resp           - responsiveness scalar
#
# Returns: scalar attractiveness weight A_ij >= 0
# =============================================================

# Attractiveness v1 — Absolute Power Difference  [original]
# A_ij = (1 - |pref_i - pref_j|) * sigmoid(r * (pow_j - pow_i))
#
# Uses raw power difference as sigmoid input. Saturates quickly once
# power differences grow large across rounds — r loses interpretive
# meaning after an early hierarchy forms.
attractiveness_absolute <- function(pref_i, pref_j, pow_i, pow_j, resp) {
  (1 - abs(pref_i - pref_j)) / (1 + exp(-resp * (pow_j - pow_i)))
}


# Attractiveness v2 — Log-Ratio Power  [current default]
# A_ij = (1 - |pref_i - pref_j|) * sigmoid(r * log(pow_j / pow_i))
#
# Uses the log of the power ratio instead of the absolute difference.
# A 5:1 power ratio produces the same sigmoid input at any absolute
# scale — r retains a consistent interpretation across all rounds.
# Safe because power >= 1 always (every agent holds their own vote).
attractiveness_log <- function(pref_i, pref_j, pow_i, pow_j, resp) {
  (1 - abs(pref_i - pref_j)) / (1 + exp(-resp * log(pow_j / pow_i)))
}


# =============================================================
# (B) SELF-WEIGHT FORMULAS
#
# Interface (identical across all versions):
#   i              - integer index of the focal agent
#   nb             - integer vector of out-neighbour indices
#   pref           - numeric vector of all agents' preferences
#   pow            - numeric vector of all agents' current power
#   responsiveness - sigmoid steepness scalar
#
# Returns: scalar w_self in (0, 1)
# =============================================================

# Self-weight Formula 1 — Absolute Power  [archived]
# w_self = sigmoid(r * own_power)
#
# Self-confidence proportional to own absolute power, no social
# comparison. At high r, even power = 1 gives w_self ≈ 1, suppressing
# delegation before any power hierarchy can form.
selfweight_absolute_power <- function(i, nb, pref, pow, responsiveness) {
  1 / (1 + exp(-responsiveness * pow[i]))
}


# Self-weight Formula 2 — Mean Contest  [archived]
# w_self = mean over j of sigmoid(r * (power_i - power_j))
#
# Averages win-probabilities against all neighbours. Ideology-blind:
# a powerful ideologically distant neighbour suppresses self-weight
# just as much as an ideologically close one.
selfweight_mean_contest <- function(i, nb, pref, pow, responsiveness) {
  if (!length(nb)) return(0.5)
  mean(1 / (1 + exp(-responsiveness * (pow[i] - pow[nb]))))
}


# Self-weight Formula 3 — Best-Neighbour Argmax, Absolute  [archived]
# j* = argmax A_ij (absolute),  w_self = sigmoid(r * (pow_i - pow_j*))
#
# Two-step logic using absolute power differences throughout.
# Kept for comparison with the log-ratio version below.
selfweight_argmax <- function(i, nb, pref, pow, responsiveness) {
  if (!length(nb)) return(0.5)
  a_nb   <- (1 - abs(pref[i] - pref[nb])) *
            (1 / (1 + exp(-responsiveness * (pow[nb] - pow[i]))))
  j_star <- nb[which.max(a_nb)]
  1 / (1 + exp(-responsiveness * (pow[i] - pow[j_star])))
}


# Self-weight Formula 4 — Best-Neighbour Argmax, Log-Ratio  [current default]
# j* = argmax A_ij (log-ratio),  w_self = sigmoid(r * log(pow_i / pow_j*))
#
# Same two-step logic as Formula 3, but uses log-ratio power throughout:
#   Step 1 — j* identified via attractiveness_log (ideology x log-ratio power)
#   Step 2 — self-weight via sigmoid(r * log(pow_i / pow_j*))
#
# A 5:1 power ratio produces the same weight at any absolute scale,
# so r retains a consistent interpretation across all simulation rounds.
# Fallback: w_self = 0.5 when agent has no neighbours.
selfweight_argmax_log <- function(i, nb, pref, pow, responsiveness) {
  if (!length(nb)) return(0.5)
  a_nb   <- (1 - abs(pref[i] - pref[nb])) *
            (1 / (1 + exp(-responsiveness * log(pow[nb] / pow[i]))))
  j_star <- nb[which.max(a_nb)]
  1 / (1 + exp(-responsiveness * log(pow[i] / pow[j_star])))
}
