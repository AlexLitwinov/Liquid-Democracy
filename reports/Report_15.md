Weekly Report – Week 14: Minority Representation in Liquid Democracy
================
2026-05-24

------------------------------------------------------------------------

# 1 Minority Representation

Each lay agent is assigned a permanent binary group label (majority /
minority). The core question is whether the emergent delegation
structure over- or under-represents the minority, measured by
$RR_m = \text{minority power share} / \text{minority population share}$.

**Baseline + three conditions** design:

- **Baseline** ($r_{ingroup} = 0$, every other r = 1 or varrying at a
  time, standard WS network): no ingroup preference, random minority
  opinions — establishes the representation level produced by opinion
  and power dynamics alone.
- **Condition A** ($r_{ingroup}$ varied, random minority opinions):
  behavioural ingroup preference is switched on while minority opinions
  remain uniformly distributed.
- **Condition B** ($r_{ingroup}$ varied, $p_{ingroup}$ clustered
  minority opinions ($\mu_m$ and $\sigma_m$): same as A but the minority
  is ideologically coherent, testing whether opinion cohesion has an
  effect on minority representation.
- **Condition C** ($r_{ingroup}$, $p_{ingroup}$ varied and clustered
  minority opinions): separates behavioural homophily from structural
  network homophily, and examines their interaction.

------------------------------------------------------------------------

# 2 Model Extension

## 2.1 Group Identity

Each lay agent is assigned a binary group label at initialisation:
$g_i \in \{\text{majority}, \text{minority}\}$. Group membership is
permanent – agents cannot change groups.

## 2.2 Extended Attractiveness Formula

The baseline attractiveness is extended by a third multiplicative term:

$$\tilde{A}_{ij}(t) = \sigma\left(r_{op}(1-2|o_i-o_j|)\right) \cdot
  \sigma\left(r_{pw}\log\frac{p_j}{p_i}\right) \cdot
  \sigma\left(r_{ingroup} \cdot I(g_i=g_j)\right)$$

where

$$I(g_i = g_j) = \begin{cases} +1 & g_i = g_j \\ -1 & g_i \neq g_j \end{cases}$$

**Properties:**

- $r_{ingroup} = 0$: $\sigma(0) = 0.5$ for all pairs – ingroup term is
  constant, recovering the Report 13 baseline exactly.
- $r_{ingroup} > 0$: ingroup pairs ($I = +1$) receive a weight boost
  $\sigma(r_{ingroup}) > 0.5$; outgroup pairs are discounted
  $\sigma(-r_{ingroup}) < 0.5$.

## 2.3 Structural Homophily (Condition C)

The Watts-Strogatz algorithm builds the network by rewiring each edge
once with probability $\beta$. In the standard case the replacement
target is drawn uniformly at random. Here that draw is replaced by a
**weighted sample**: each candidate $j$ receives weight

$$w_j = \begin{cases} e^{p_{ingroup}} & g_j = g_i \\ 1 & g_j \neq g_i \end{cases}$$

Because ingroup nodes carry a larger weight, rewired edges land
disproportionately within the same group. The higher $p_{ingroup}$, the
stronger the bias — at $p_{ingroup} = 0$ the draw is uniform and the
standard WS topology is recovered. The resulting network has denser
within-group connectivity before any agent makes a delegation decision.

**Measuring structural homophily — Coleman’s index $h_m$.** The
parameter $p_{ingroup}$ controls how strongly the rewiring is biased,
but it is not directly interpretable as a share of ingroup connections.
To get a concrete, comparable measure of the resulting network topology
we compute **Coleman’s homophily index** for the minority group after
each graph is built:

$$h_m = \frac{f_{mm} - m}{1 - m}$$

where $f_{mm}$ is the average fraction of a minority agent’s neighbours
that are also minority, and $m = 0.2$ is the minority population share.
Under a random network $f_{mm} \approx m$, so $h_m \approx 0$. When all
minority agents connect exclusively to other minority agents,
$f_{mm} = 1$ and $h_m = 1$. The index therefore measures how much more
ingroup-connected the minority is relative to what pure chance would
produce, on a $[0, 1]$ scale that is comparable across different $N$ and
$k$ settings.

$h_m$ is recorded as a network-level output variable at the start of
each simulation run (before any delegation takes place) and is used in
Topic 3 as the structural axis, alongside $p_{ingroup}$ as the input
parameter.

------------------------------------------------------------------------

# 3 Minority-Specific Metrics

**Minority power share** $= \sum_{i \in M} p_i / \sum_j p_j$: fraction
of total votes held by minority agents.

**Relative representation ratio**
$RR_m = (\text{minority power share}) / (\text{minority population share})$:
the key outcome variable. $RR_m = 1$ is proportional representation;
$RR_m < 1$ is underrepresentation; $RR_m > 1$ is overrepresentation.

**Minority ENP**
$= 1 / \sum_{i \in M_{\text{voters}}} (p_i / \sum_{j \in M_{\text{voters}}} p_j)^2$:
effective number of independent minority power-holders. Low minority ENP
means minority votes are concentrated in one or a few superdelegates.

**Cross-group delegation rate**: fraction of all delegation edges
pointing from one group to the other. Measures group integration in the
delegation graph.

**Minority average chain length**: mean delegation chain length for
delegating minority agents. Long minority chains indicate deep nesting
under majority delegates.

**Minority ideological drift**: mean $|o_i - v_j(t)|$ among minority
agents whose vote is represented. Measures how far minority-cast votes
deviate from minority opinions.

**Minority total Components**: Number of direct voters among minority
agents.

------------------------------------------------------------------------

# 4 Research Topics

Three outcome dimensions are investigated across all conditions
(Baseline, A, B, C):

------------------------------------------------------------------------

**Topic 1 — Minority representation**

How are minorities represented in the final vote count, and how does
this change under ingroup preference, ideological cohesion, and
structural homophily?

- *Metrics:* $RR_m$, `minority_power_share`, `minority_enp`

------------------------------------------------------------------------

**Topic 2 — Delegation behaviour**

How do minorities delegate — within or across group boundaries — and
does this change across conditions?

- *Metrics:* `cross_group_dlg_rate`, `minority_chain_len`, `coleman_h`

------------------------------------------------------------------------

**Topic 3 — Ideological drift**

How far do minority votes deviate from actual minority opinions, and
under which conditions does this increase or decrease?

- *Metrics:* `minority_drift`

------------------------------------------------------------------------

**Topic 4 — Network moderators**

Do $N$ and $k$ amplify or dampen the effects in Topics 1–3?

- *Metrics:* all above, stratified by $N$ and $k$
