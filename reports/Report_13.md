Weekly Report – Week 13 (15.05.2026 – 21.05.2026)
================
2026-05-15

## Design

Two simulation sweeps underpin this report.

**Large sweep** ($r_{op}, r_{pw} \in \{0,\ldots,15\}$, N $\in$ {300,
1000}, 25 seeds per cell): used for GAM overview plots that capture the
global, potentially nonlinear response surface.

**Robustness sweep** ($r_{op}, r_{pw} \in \{0,1,2\}$, N $\in$ {100, 200,
, 1000}, 100 seeds per cell): used for LMM inference (N = 300 subset)
and N-scaling analysis across all ten network sizes.

All metrics are averaged over simulation rounds 15–20.

    ## Large sweep:    66 min 36.2 sec  |  512 parameter combinations  |  12800 simulations

    ## Robust sweep:   39 min 55.1 sec  |  90 parameter combinations  |  9000 simulations

------------------------------------------------------------------------

# GAM Overview – Delegation & Lost Vote Rate

![](Report_13_files/figure-gfm/gam-ov-lineplot-1.png)<!-- -->

Checked for reasonable r values. Plots show a very high delegation +
lost vote rate for any value higher than r = 2. Therefore for the
following simulations + models r values in {0, 1, 2}.

------------------------------------------------------------------------

# Part 1 – N = 300

## 0 Preliminary Check – Seed-Level Bias

Each seed defines a unique network topology (Watts-Strogatz graph) and
initial opinion vector. If certain seeds consistently produce higher
delegation rates or longer chains regardless of parameter settings,
ignoring this structure inflates model residuals. The mixed model in
Part 1 includes a seed-level random intercept `(1|seed)` to absorb this
variation.

![](Report_13_files/figure-gfm/seed-bias-check-1.png)<!-- -->

| Metric                 | Grand mean |     SD |     CV |
|:-----------------------|-----------:|-------:|-------:|
| Avg. Chain Length      |     2.7157 | 0.2010 | 0.0740 |
| Avg. Ideological Drift |     0.2482 | 0.0106 | 0.0426 |
| Delegation Rate        |     0.9097 | 0.0044 | 0.0049 |
| ENP                    |    39.5779 | 5.2716 | 0.1332 |
| Gini Coefficient       |     0.3857 | 0.0086 | 0.0224 |
| Lost Vote Rate         |     0.6399 | 0.0189 | 0.0296 |
| Top-5% Power Share     |     0.2743 | 0.0155 | 0.0567 |
| Total Components       |    45.9467 | 1.7064 | 0.0371 |

Seed-level variability – CV \> 0.05 indicates meaningful seed bias
warranting a random intercept.

CV values above 0.05 confirm that network topology and initial opinions
introduce non-negligible between-seed variation, justifying the random
intercept `(1|seed)`.

------------------------------------------------------------------------

## 1 LMM – fine grid (r ∈ {0, 1, 2}, N = 300)

Within the approximately linear regime ($r \leq 2$) the interaction
model

$$y_{ijk} = \beta_0 + \beta_1\,r_{op} + \beta_2\,r_{pw} + \beta_3\,(r_{op} \cdot r_{pw}) + u_k + \varepsilon_{ijk}$$

is fitted for all seven metrics simultaneously (delegation dynamics and
power concentration). $u_k \sim \mathcal{N}(0, \sigma_u^2)$ is the seed
random intercept.

- **ENP** ($1/\sum s_i^2$): effective number of equally powerful agents;
  *higher = less concentrated*

| Metric | b1 (r_op) | b2 (r_pw) | b3 (r_op:r_pw) | R2m | R2c |
|:---|:---|:---|:---|---:|---:|
| Avg. Chain Length | 0.1605\*\* (\< .001) | -0.0429 (0.118) | 0.0089 (0.675) | 0.090 | 0.182 |
| Delegation Rate | 0.0497\*\* (\< .001) | 0.0023\*\* (\< .001) | -0.005\*\* (\< .001) | 0.956 | 0.966 |
| Lost Vote Rate | 0.1521\*\* (\< .001) | -0.0426\*\* (\< .001) | 0.0011 (0.543) | 0.922 | 0.934 |
| Gini Coefficient | 0.007\*\* (\< .001) | 0.0082\*\* (\< .001) | -0.0014 (0.063) | 0.152 | 0.291 |
| Avg. Ideological Drift | 0.0074\*\* (\< .001) | 0.009\*\* (\< .001) | -0.002\*\* (0.004) | 0.159 | 0.424 |
| Top-5% Power Share | -0.0017 (0.444) | 0.0039 (0.083) | 0.0026 (0.127) | 0.024 | 0.105 |
| ENP | -20.1163\*\* (\< .001) | 3.0745\*\* (\< .001) | 0.5936 (0.137) | 0.757 | 0.817 |
| Total Components | -4.5229\*\* (\< .001) | 0.2971 (0.094) | 0.3238\* (0.019) | 0.556 | 0.652 |

LMM fixed effects – all metrics, r in {0,1,2,3}, N = 300. \* p \< 0.05,
\*\* p \< 0.01. R2m = marginal (fixed only), R2c = conditional (fixed +
seed).

**Per-metric interpretation:**

**Delegation Dynamics: Delegation Rate and Lost Vote Rate** — Opinion
responsiveness strongly increases both delegation activity and the
likelihood of votes becoming lost within delegation chains. Agents are
therefore substantially more likely to delegate when ideological
similarity is prioritized, but this simultaneously creates more fragile
delegation structures in which votes fail to reach an active
representative. In contrast, power responsiveness slightly stabilizes
the system by reducing lost votes, likely because delegations are
directed toward already central and consistently active agents. Overall,
the results suggest that ideological delegation promotes participation
but also increases the lost vote rate due to cycles. **Delegation
Structure: Average Chain Length and Ideological Drift** - Higher opinion
responsiveness produces longer delegation chains, indicating that agents
delegate through sequences of ideologically similar intermediaries
rather than directly to highly central actors. At the same time, both
opinion responsiveness and power responsiveness increase (slightly)
ideological drift, implying that indirect delegation can weaken
representational fidelity between voters and final representatives.
However, the negative interaction effect suggests that the simultaneous
presence of both mechanisms partially constrains this increase in drift,
whereas this effect almost 0. Together, these findings indicate that
decentralized delegation dynamics can generate increasingly indirect and
structurally complex forms of representation. But it needs to be said
that those effects are relatively small! **Power Concentration: Gini
Coefficient, Top-5% Share, and ENP** — Both responsiveness to public
opinion and responsiveness to power contribute to a slight increase in
overall inequality of political influence, although the effects are
generally small. However, they act along different structural
dimensions: opinion responsiveness strongly reduces the effective number
of influential agents, which is most likely due to higher lost vote
rate, while power responsiveness tends to distribute influence across
multiple competing hubs rather than reinforcing a single dominant
center. The absence of strong effects on the Top-5% power share suggests
that concentration emerges more gradually across the system rather than
through extreme domination by a very small elite. The absence of strong
effects on the Top-5% power share suggests that concentration does not
primarily take the form of extreme winner-takes-all (“rich-get-richer”)
dynamics at the very top. Overall, the results demonstrate that local
delegation preferences can generate macroscopic inequalities in
political influence through self-reinforcing network dynamics.

![](Report_13_files/figure-gfm/lmm-marginal-plots-1.png)<!-- -->

![](Report_13_files/figure-gfm/lmm-heatmaps-1.png)<!-- -->

Close agreement between predicted and observed heatmaps confirms that
the linear interaction model captures the dominant structure within
$r \leq 3$. Systematic deviations (e.g. curvature in corners) would
indicate residual nonlinearity not captured by $\beta_3$.

------------------------------------------------------------------------

# Part 3 – Robustness: Effect of Network Size N

The robustness sweep covers N $\in$ {100, 200, , 1000} at
$r_{op}, r_{pw} \in \{0,1,2,3\}$. Three complementary analyses address
whether the parameter effects are N-dependent: (1) raw metric
trajectories at a fixed reference point, (2) N included directly as a
predictor to estimate the per-unit effect, and (3) separate LMMs at N =
200, 500, 1000 compared side by side.

ENP and Total Components are normalised for Part 3 comparisons across N:
ENP is divided by the number of direct voters ($n_\text{voters}$) and
Total Components by $N$.

------------------------------------------------------------------------

## 3.1 Metric trends across N (r_op = r_pw = 1)

![](Report_13_files/figure-gfm/n-trends-1.png)<!-- -->

Across the explored range ($N = 200$–$1000$), most delegation and
concentration metrics remain relatively stable, suggesting that the
system operates in a largely size-invariant regime under
$r_{op} = r_{pw} = 1$.

The strongest size dependence appears in ENP, Top-5% Power Share and
Avg. Chain Length (for smaller sizes).

------------------------------------------------------------------------

## 3.2 N as a predictor

To estimate *how much* each metric changes per unit increase in N –
controlling for $r_{op}$ and $r_{pw}$ – the model
$y \sim r_{op} \times r_{pw} + N$ is fitted on cell means (averaged over
seeds within each parameter combination). The coefficient for N is
directly interpretable as the expected change per additional agent.

| Metric | b (N) | p | SE | t | 95% CI low | 95% CI high |
|:---|:---|:---|---:|---:|---:|---:|
| Avg. Chain Length | 0.000241\*\* | \< .001 | 3.5e-05 | 6.989 | 0.000173 | 3.1e-04 |
| Delegation Rate | 0 | 0.918 | 2.0e-06 | -0.103 | -0.000004 | 4.0e-06 |
| Lost Vote Rate | -2e-06 | 0.751 | 7.0e-06 | -0.318 | -0.000016 | 1.2e-05 |
| Gini Coefficient | 2e-05\*\* | \< .001 | 3.0e-06 | 6.893 | 0.000014 | 2.6e-05 |
| Avg. Ideological Drift | 1.1e-05\*\* | \< .001 | 2.0e-06 | 6.203 | 0.000007 | 1.4e-05 |
| Top-5% Power Share | 3.6e-05\*\* | \< .001 | 5.0e-06 | 6.939 | 0.000026 | 4.7e-05 |
| ENP | -7.3e-05\*\* | \< .001 | 8.0e-06 | -9.050 | -0.000089 | -5.7e-05 |
| Total Components | 0 | 0.822 | 1.0e-06 | 0.226 | -0.000001 | 2.0e-06 |

Effect of N on each metric, controlling for r_op \* r_pw. Model: lm(y ~
r_op \* r_pw + n_agents) fitted on cell means. b(N) = estimated change
per 1-unit increase in N. \* p \< 0.05, \*\* p \< 0.01.

Some metrics vary partially around a mean while others appear to need to
exceed a certain threshold before levelling off.

------------------------------------------------------------------------

## 3.3 Coefficient tables – N = 200, 500, 1000

| Metric | N | b1 (r_op) | b2 (r_pw) | b3 (r_op:r_pw) | R2m | R2c |
|:---|---:|:---|:---|:---|---:|---:|
| Avg. Chain Length | 200 | 0.1074\*\* (0.006) | -0.0086 (0.825) | 0.0059 (0.844) | 0.037 | 0.215 |
| Avg. Chain Length | 500 | 0.1038\*\* (\< .001) | -0.0716\* (0.016) | 0.0373 (0.105) | 0.107 | 0.229 |
| Avg. Chain Length | 1000 | 0.1636\*\* (\< .001) | -0.0728\*\* (\< .001) | 0.006 (0.705) | 0.274 | 0.370 |
| Avg. Ideological Drift | 200 | 0.0059\*\* (\< .001) | 0.0097\*\* (\< .001) | -0.0028\* (0.037) | 0.085 | 0.234 |
| Avg. Ideological Drift | 500 | 0.0064\*\* (\< .001) | 0.0085\*\* (\< .001) | -0.002\* (0.012) | 0.199 | 0.445 |
| Avg. Ideological Drift | 1000 | 0.0066\*\* (\< .001) | 0.0086\*\* (\< .001) | -0.0022\*\* (\< .001) | 0.378 | 0.548 |
| Delegation Rate | 200 | 0.0482\*\* (\< .001) | 0.0015\* (0.041) | -0.0042\*\* (\< .001) | 0.938 | 0.956 |
| Delegation Rate | 500 | 0.0487\*\* (\< .001) | 0.0018\*\* (0.002) | -0.0048\*\* (\< .001) | 0.966 | 0.970 |
| Delegation Rate | 1000 | 0.0493\*\* (\< .001) | 0.0025\*\* (\< .001) | -0.005\*\* (\< .001) | 0.973 | 0.976 |
| ENP | 200 | -0.0043 (0.634) | -0.0175 (0.053) | 0.0023 (0.746) | 0.013 | 0.214 |
| ENP | 500 | -0.0111 (0.075) | -0.0106 (0.088) | -0.0029 (0.552) | 0.046 | 0.153 |
| ENP | 1000 | -0.0234\*\* (\< .001) | -0.0077 (0.071) | 0.0013 (0.703) | 0.124 | 0.243 |
| Gini Coefficient | 200 | 0.0021 (0.305) | 0.0091\*\* (\< .001) | -0.0011 (0.478) | 0.058 | 0.313 |
| Gini Coefficient | 500 | 0.0048\*\* (\< .001) | 0.0085\*\* (\< .001) | -9e-04 (0.333) | 0.190 | 0.373 |
| Gini Coefficient | 1000 | 0.008\*\* (\< .001) | 0.0078\*\* (\< .001) | -0.0014\* (0.011) | 0.441 | 0.491 |
| Lost Vote Rate | 200 | 0.1493\*\* (\< .001) | -0.0462\*\* (\< .001) | 0.0042 (0.156) | 0.886 | 0.909 |
| Lost Vote Rate | 500 | 0.1499\*\* (\< .001) | -0.0453\*\* (\< .001) | 0.0011 (0.589) | 0.939 | 0.949 |
| Lost Vote Rate | 1000 | 0.1538\*\* (\< .001) | -0.0419\*\* (\< .001) | -3e-04 (0.851) | 0.959 | 0.963 |
| Top-5% Power Share | 200 | 0.0024 (0.535) | 0.0081\* (0.038) | -9e-04 (0.764) | 0.016 | 0.210 |
| Top-5% Power Share | 500 | 5e-04 (0.823) | 0.0032 (0.190) | 0.0034 (0.068) | 0.051 | 0.190 |
| Top-5% Power Share | 1000 | 0.0095\*\* (\< .001) | 0.0027 (0.090) | 0.001 (0.438) | 0.202 | 0.250 |
| Total Components | 200 | -0.0147\*\* (\< .001) | 0.0012 (0.181) | 0.001 (0.146) | 0.478 | 0.628 |
| Total Components | 500 | -0.0146\*\* (\< .001) | 0.0014\* (0.025) | 8e-04 (0.103) | 0.664 | 0.752 |
| Total Components | 1000 | -0.014\*\* (\< .001) | 0.0011\* (0.016) | 7e-04 (0.064) | 0.798 | 0.834 |

LMM coefficients for N = 200, 500, 1000 (r in {0,1,2,3}). \* p \< 0.05,
\*\* p \< 0.01

Across all system sizes, delegation rate and lost vote dynamics remain
highly stable and strongly explained by the responsiveness parameters
($R^2_m \approx 0.95$), indicating robust and largely scale-independent
delegation behavior. In contrast, chain length, ideological drift, and
power concentration become increasingly sensitive to the parameters as
$N$ grows, reflected in rising effect sizes and higher explanatory power
at larger population sizes.

------------------------------------------------------------------------

## 3.4 Coefficient plot – N = 200, 500, 1000

![](Report_13_files/figure-gfm/n-coef-plot-1.png)<!-- -->

------------------------------------------------------------------------

# Part 2 – Trust-Weighted Delegation

## Formulas

**Trust update** (after each round, using the effective vote propagated
through $j$’s chain):

$$\tau_{ij}(t) = \lambda \cdot \tau_{ij}(t-1) - (1-\lambda)\cdot\bigl|o_i - v_j(t-1)\bigr|$$

**Modified attractiveness:**

$$\tilde{A}_{ij}(t) = A_{ij} \cdot 2\sigma\bigl(\gamma\cdot\tau_{ij}(t)\bigr)$$

where
$A_{ij} = \sigma\!\left(r_{op}(1-2|o_i-o_j|)\right)\cdot\sigma\!\left(r_{pw}\log(p_j/p_i)\right)$
is the baseline attractiveness from Part 1. The factor $2\sigma(\cdot)$
ensures that $\tau=0$ (no experience) gives a neutral modifier of
$2\sigma(0)=1$, recovering the unmodified $A_{ij}$ exactly. The
self-weight $w_\text{self}$ is unchanged.

The simulations in Part 1 and Part 3 used $\lambda = 0$, $\gamma = 0$,
which sets the modifier to $2\sigma(0)=1$ for all agents and rounds,
leaving the original attractiveness intact.

Here, $\lambda = 0.5$ is held fixed (exponential moving average with
half-life of one round) and $\gamma$ is varied from 0 to 3 to examine
how trust sensitivity modulates delegation patterns.

## Effect of Trust Sensitivity $\gamma$ on All Metrics

$\lambda = 0.5$ fixed; $\gamma \in [0, 3]$;
$r_{op}, r_{pw} \in \{0, 1, 2\}$. Points are seed-averaged cell means;
lines connect them. Colour encodes $r_{op}$, line type encodes $r_{pw}$.

![](Report_13_files/figure-gfm/trust-gamma-plot-1.png)<!-- -->

## LMM with Trust – Full Interaction Model

The same mixed-effects framework as in Part 1 is extended to include
$\gamma$ as a third factorial predictor:

$$y_{ijk} = \beta_0 + \beta_1 r_{op} + \beta_2 r_{pw} + \beta_\gamma \gamma + \beta_3 (r_{op} \cdot r_{pw}) + \beta_4 (r_{op} \cdot \gamma) + \beta_5 (r_{pw} \cdot \gamma) + \beta_6 (r_{op} \cdot r_{pw} \cdot \gamma) + u_k + \varepsilon_{ijk}$$

$\lambda = 0.5$ is held fixed throughout. The LRT tests whether the
three-way interaction $\beta_6$ significantly improves fit over the
model containing all two-way interactions but no three-way term.

| Metric | b1 (r_op) | b2 (r_pw) | b_γ (γ) | b3 (r_op:r_pw) | b4 (r_op:γ) | b5 (r_pw:γ) | b6 (r_op:r_pw:γ) | R2m | R2c |
|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|
| Avg. Chain Length | 0.1643\*\* (\< .001) | -0.047\*\* (0.007) | -0.0984\*\* (\< .001) | -0.0029 (0.828) | 0.0204\* (0.034) | 0.0069 (0.474) | -0.0071 (0.343) | 0.159 | 0.179 |
| Delegation Rate | 0.0477\*\* (\< .001) | 6e-04 (0.109) | -0.028\*\* (\< .001) | -0.0041\*\* (\< .001) | 0.0102\*\* (\< .001) | 0.0063\*\* (\< .001) | -0.0038\*\* (\< .001) | 0.965 | 0.970 |
| Lost Vote Rate | 0.1513\*\* (\< .001) | -0.0456\*\* (\< .001) | -0.057\*\* (\< .001) | 0.0022 (0.082) | 0.0169\*\* (\< .001) | 0.0088\*\* (\< .001) | -0.0067\*\* (\< .001) | 0.935 | 0.940 |
| Gini Coefficient | 0.0056\*\* (\< .001) | 0.0078\*\* (\< .001) | -0.0072\*\* (\< .001) | -0.0014\* (0.012) | 0.0023\*\* (\< .001) | 0.0016\*\* (\< .001) | -9e-04\*\* (0.002) | 0.245 | 0.281 |
| Avg. Ideological Drift | 0.006\*\* (\< .001) | 0.0088\*\* (\< .001) | -0.0066\*\* (\< .001) | -0.0016\*\* (\< .001) | 0.001\*\* (0.002) | 9e-04\*\* (0.005) | -8e-04\*\* (0.002) | 0.266 | 0.415 |
| Top-5% Power Share | -0.0021 (0.159) | 0.0031\* (0.037) | -0.0083\*\* (\< .001) | 0.0017 (0.135) | 0.004\*\* (\< .001) | 9e-04 (0.293) | -8e-04 (0.180) | 0.045 | 0.070 |
| ENP | -19.1468\*\* (\< .001) | 4.3748\*\* (\< .001) | 9.967\*\* (\< .001) | -0.0789 (0.802) | -3.3527\*\* (\< .001) | -1.5289\*\* (\< .001) | 1.1205\*\* (\< .001) | 0.807 | 0.826 |
| Total Components | -4.1781\*\* (\< .001) | 0.5344\*\* (\< .001) | 2.512\*\* (\< .001) | 0.1287 (0.208) | -0.7599\*\* (\< .001) | -0.3784\*\* (\< .001) | 0.2757\*\* (\< .001) | 0.656 | 0.680 |

LMM with trust: y ~ r_op \* r_pw \* gamma + (1\|seed), lambda = 0.5
fixed, N = 300. \* p \< 0.05, \*\* p \< 0.01. LRT X2 tests the three-way
term against the full two-way model.
