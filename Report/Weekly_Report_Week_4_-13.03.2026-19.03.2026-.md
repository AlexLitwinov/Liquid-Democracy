Weekly Report — Week 4 (13.03.2026 – 19.03.2026)
================
2026-03-13

## Summary

This week focused on investigating the convergence behaviour of the
Liquid Democracy model and extending the simulation with an inertia
mechanism to model delegation persistence.

------------------------------------------------------------------------

## Work Done

**Model Extension — Inertia Parameter**

- Added `inertia` as a new parameter to `simulate_liquid_democracy()`
  with default value `inertia = 0` (backward compatible, no effect on
  Experiment 1 results)
- Inertia is implemented as a retention probability: at the start of
  each round, an agent who delegated in the previous round keeps their
  current delegate with probability `inertia`, bypassing the normal
  attractiveness calculation entirely
- At `inertia = 0` the model behaves identically to Experiment 1; at
  `inertia = 1` agents never switch delegates after the first round,
  guaranteeing structural convergence
- This design makes `inertia` directly interpretable as a probability
  and scale-invariant with respect to `responsiveness`

------------------------------------------------------------------------

## Experiment 2 — Convergence Analysis

### Design

| Parameter                | Values                  |
|--------------------------|-------------------------|
| Responsiveness           | 0.1, 1, 5, 25           |
| Rewiring probability (p) | 0, 0.05, 0.25           |
| Delegation rounds T      | 2000                    |
| Inertia                  | 0 (memoryless baseline) |

Three metrics were tracked at every round for each parameter
combination:

- **Delegation stability**: fraction of lay agents not switching their
  delegation target between consecutive rounds (1.0 = fully converged)
- **Edge similarity**: Jaccard index between the delegation graphs of
  consecutive rounds — measures structural similarity at the network
  level
- **Lost vote rate**: fraction of votes lost to cycles at each round

### Key Results

**Delegation Stability**

The system does not converge structurally under the memoryless baseline
(`inertia = 0`). Delegation stability fluctuates around a stable mean of
approximately 0.18–0.20 at responsiveness = 0.1 and 0.30–0.40 at
responsiveness = 5 and 25, but never approaches the 99% convergence
threshold across all 2000 rounds. This confirms that the model reaches a
stochastic equilibrium — the distribution of outcomes stabilises —
rather than a fixed-point delegation structure.

**Lost Vote Rate**

The lost vote rate oscillates persistently around a stable mean without
any downward trend over time. At responsiveness = 0.1 the mean is
approximately 0.40–0.45; at responsiveness = 5 and 25 it falls to
roughly 0.05–0.10. Cycles do not self-resolve over longer time horizons
— they are an inherent feature of the memoryless probabilistic decision
rule rather than a transient startup artefact.

**Edge Similarity**

Consistent with delegation stability, Jaccard edge similarity between
consecutive rounds remains well below 1.0 throughout and shows no upward
trend. The delegation network is continuously reshuffled even when
aggregate outcome metrics such as lost vote rate and ideological drift
are stable.

### Summary

The memoryless model (`inertia = 0`) does not converge in a structural
sense. Agents continuously revise their delegation decisions every
round, producing persistent fluctuation in the delegation graph despite
stable aggregate outcomes. This motivates the inertia extension: adding
even moderate retention probability is expected to produce genuine
structural convergence, which will be studied in Experiment 3.

------------------------------------------------------------------------

## Open Issues

- Run convergence analysis with `inertia > 0` to determine how much
  memory is needed to achieve structural convergence (Experiment 3)
- Preference distribution of cycle agents: check whether agents stuck in
  cycles are systematically biased toward Yes or No, which would explain
  the liquid margin distortion observed in Experiment 1
- Multi-seed robustness check: all results currently use seed = 123
- Experiment 4 — Experts: re-run Experiment 1 design with
  `n_experts_per_community > 0` to study whether experts act as
  cycle-breakers and reduce lost vote rates
