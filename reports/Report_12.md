Weekly Report — Week 12 (08.05.2026 – 14.05.2026)
================
2026-05-08

## 1. Design

Full **6 × 6 parameter grid** ($r_{op}, r_{pw} \in \{0,\ldots,5\}$) ×
**4 agent counts** ($n \in \{100, 250, 1000, 2500\}$), 100 seeds each.
All metrics are averaged over rounds 15–20.

$$A_{ij} = \sigma\!\left(r_{op}(1-2|o_i-o_j|)\right)\cdot\sigma\!\left(r_{pw}\log\tfrac{p_j}{p_i}\right)$$

$$w_{\text{self}}(i) = \sigma\!\left(r_{op}(2|o_i-o_{j^\ast}|-1)\right)\cdot\sigma\!\left(r_{pw}\log\tfrac{p_i}{p_{j^\ast}}\right)$$

------------------------------------------------------------------------

    ## 
    ## ── grid-run ──────────────────────────────────────────
    ##   Combinations : 4 n_agents × 6 r_op × 6 r_pw = 144
    ##   Seeds        : 100
    ##   Total runs   : 14,400
    ##   Workers      : 15
    ##   Elapsed      : 226.3 min  (0.94 s / run)
    ## ──────────────────────────────────────────────────────

------------------------------------------------------------------------

## 2. Parameter Profiles ($n = 250$)

Solid lines: cross-seed mean. Ribbons: mean ± 1 SD.

![](Report_12_files/figure-gfm/line-plots-1.png)<!-- -->![](Report_12_files/figure-gfm/line-plots-2.png)<!-- -->![](Report_12_files/figure-gfm/line-plots-3.png)<!-- -->![](Report_12_files/figure-gfm/line-plots-4.png)<!-- -->![](Report_12_files/figure-gfm/line-plots-5.png)<!-- -->

------------------------------------------------------------------------

## 3. Mean & Variability Heatmaps ($n = 250$)

Left: cross-seed mean. Right: coefficient of variation (CV = SD/mean).
CV peaks at phase-boundary cells — high CV indicates that small
differences in initialisation are sufficient to tip the system between
regimes.

![](Report_12_files/figure-gfm/heatmaps-1.png)<!-- -->![](Report_12_files/figure-gfm/heatmaps-2.png)<!-- -->![](Report_12_files/figure-gfm/heatmaps-3.png)<!-- -->![](Report_12_files/figure-gfm/heatmaps-4.png)<!-- -->![](Report_12_files/figure-gfm/heatmaps-5.png)<!-- -->

------------------------------------------------------------------------

## 4. Varying N

![](Report_12_files/figure-gfm/robustness-heatmaps-1.png)<!-- -->![](Report_12_files/figure-gfm/robustness-heatmaps-2.png)<!-- -->![](Report_12_files/figure-gfm/robustness-heatmaps-3.png)<!-- -->![](Report_12_files/figure-gfm/robustness-heatmaps-4.png)<!-- -->![](Report_12_files/figure-gfm/robustness-heatmaps-5.png)<!-- -->

------------------------------------------------------------------------

## 5. Extension: Outcome-Aware Delegation via Trust

### Formulas

**Trust update** (after each round, using the effective vote propagated
through $j$’s chain):

$$\tau_{ij}(t) = \lambda \cdot \tau_{ij}(t-1) \;-\; (1-\lambda)\cdot\bigl|o_i - v_j(t-1)\bigr|$$

**Modified attractiveness:**

$$\tilde{A}_{ij}(t) = A_{ij} \cdot 2\,\sigma\!\bigl(\gamma\cdot\tau_{ij}(t)\bigr)$$

where
$A_{ij} = \sigma(r_{op}(1-2|o_i-o_j|))\cdot\sigma(r_{pw}\log(p_j/p_i))$
is the baseline attractiveness. The factor $2\sigma(\cdot)$ is chosen so
that $\tau=0$ (no experience) gives a neutral modifier of
$2\sigma(0)=1$, recovering the unmodified $A_{ij}$ exactly. The
self-weight $w_\text{self}$ is unchanged.

| Parameter | Interpretation | Values tested |
|----|----|----|
| $\lambda \in [0,1]$ | Memory length — how much past experience persists | 0.1, 0.5, 0.9 |
| $\gamma \geq 0$ | Trust sensitivity — how strongly trust history overrides static $A_{ij}$ | 0, 1, 2, 3 |
| $r_{op}$ | Opinion sensitivity (fixed) | 2 |
| $r_{pw}$ | Power sensitivity (fixed) | 2 |

------------------------------------------------------------------------

    ## 
    ## ── trust-run ─────────────────────────────────────────
    ##   Combinations : 3 lambda × 4 gamma = 12
    ##   Seeds        : 100
    ##   Total runs   : 1,200
    ##   Workers      : 15
    ##   Elapsed      : 1.3 min  (0.06 s / run)
    ## ──────────────────────────────────────────────────────

------------------------------------------------------------------------

## 6. Trust Dynamics Over Time

Each panel shows one metric over 20 rounds for $\gamma \in \{0,1,2,3\}$
(colour) and $\lambda \in \{0.1, 0.5, 0.9\}$ (facet rows). Ribbons: mean
± 1 SD across 100 seeds. $\gamma = 0$ is the baseline (no trust
modulation).

![](Report_12_files/figure-gfm/time-series-1.png)<!-- -->![](Report_12_files/figure-gfm/time-series-2.png)<!-- -->![](Report_12_files/figure-gfm/time-series-3.png)<!-- -->![](Report_12_files/figure-gfm/time-series-4.png)<!-- -->![](Report_12_files/figure-gfm/time-series-5.png)<!-- -->

------------------------------------------------------------------------

## 7. Steady-State Trust Parameter Profiles

Mean over rounds 15–20 (steady-state window), averaged across 100 seeds.
x-axis: $\gamma$ (trust sensitivity); colour: $\lambda$ (memory length).
Ribbons: mean ± 1 SD.

## ![](Report_12_files/figure-gfm/ss-line-plots-1.png)<!-- -->![](Report_12_files/figure-gfm/ss-line-plots-2.png)<!-- -->![](Report_12_files/figure-gfm/ss-line-plots-3.png)<!-- -->![](Report_12_files/figure-gfm/ss-line-plots-4.png)<!-- -->![](Report_12_files/figure-gfm/ss-line-plots-5.png)<!-- -->

## 8. Steady-State Comparison: Trust Sensitivity × Parameter Combinations

Steady-state means (rounds 15–20) for all $3 \times 3$ combinations of
$r_{op}, r_{pw} \in \{0,1,2\}$ as $\gamma$ increases from 0 to 3, with
$\lambda = 0.5$ fixed throughout. Each line is one $(r_{op}, r_{pw})$
pair — colour encodes $r_{op}$, line type encodes $r_{pw}$. $\gamma = 0$
is the no-trust baseline. Averaged over 100 seeds per cell.

    ## 
    ## ── ts-run ────────────────────────────────────────────
    ##   Combinations : 3 r_op × 3 r_pw × 4 gamma = 36
    ##   Seeds        : 100
    ##   Total runs   : 3,600
    ##   Workers      : 15
    ##   Elapsed      : 3.5 min  (0.06 s / run)
    ## ──────────────────────────────────────────────────────

![](Report_12_files/figure-gfm/ts-plots-1.png)<!-- -->

    ## 
    ## ══ SIMULATION SUMMARY ════════════════════════════════
    ##   grid-run   : 14,400 runs  —  226.3 min
    ##   trust-run  :  1,200 runs  —  1.3 min
    ##   ts-run     :  3,600 runs  —  3.5 min
    ##   ─────────────────────────────────────────────────────
    ##   TOTAL      : 19,200 runs  —  231.0 min  (3.9 h)
    ## ══════════════════════════════════════════════════════
