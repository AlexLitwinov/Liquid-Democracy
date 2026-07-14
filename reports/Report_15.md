Weekly Report – Week 15: Minority Representation in Liquid Democracy
================
2026-05-24

------------------------------------------------------------------------

# 1 Research Design

## 1.1 The Minority as a Political Preference Minority

The minority is defined as a **political preference minority**: a group
whose opinions are systematically different from the majority. Opinions
are drawn at initialisation from group-specific normal distributions
centred symmetrically around the opinion midpoint 0.5:

$$o_i \sim \begin{cases}
  \mathcal{N}(0.5 - \delta/2,\; 0.1^2) & g_i = \text{minority} \quad (20\,\%) \\
  \mathcal{N}(0.5 + \delta/2,\; 0.1^2) & g_i = \text{majority} \quad (80\,\%)
\end{cases}$$

where $\delta = \mu_M - \mu_m$ is the **polarisation parameter** — the
distance between the two group means. Group membership is permanent.

## 1.2 Experimental Design

Three factors are crossed. Condition determines *how* group membership
relates to opinion and to network position; Polarisation and Ingroup
preference are then varied within the conditions where they apply.

| Factor | Values | Meaning |
|----|----|----|
| Condition | $\{\text{random},\; \text{polarised},\; \text{clustered}\}$ | Group-opinion / group-network correlation regime (see below) |
| Polarisation $\delta$ | $\{0.2,\; 0.4\}$ (n/a under `random`) | Low: N(0.4) vs N(0.6) — High: N(0.3) vs N(0.7) |
| Ingroup preference $r_{ingroup}$ | $\{0, 1, 2, 4, 8\}$ | Behavioural ingroup bias, fully crossed with all three conditions |

**Condition levels**

- **random**: opinions are drawn from a single shared distribution for
  all agents regardless of group ($\delta$ not applicable), and network
  structure (neighbors) is independent of group. This is the baseline:
  any representation gap at $r_{ingroup} > 0$ is attributable to pure
  behavioural ingroup bias alone, with no opinion or structural
  confound.

- **polarised**: the existing group-specific opinion split applies
  ($\delta \in \{0.2, 0.4\}$), but network structure remains independent
  of group. This adds the *opinion-similarity* channel — agents can
  cluster by group indirectly, via opinion-based attraction ($r_{op}$),
  even at $r_{ingroup} = 0$.

- **clustered**: same opinion split as `polarised`, but agents are
  reshuffled onto the already-built network — same edges, same degree
  sequence, just a different assignment of which agent sits at which
  position — until the group-edge assortativity exceeds $r > 0.9$. This
  adds the *structural-access* channel — even at $r_{ingroup} = 0$, most
  available neighbours already share the agent’s group.

------------------------------------------------------------------------

# 2 Model Extension

## 2.1 Group Identity

Each lay agent is assigned a binary group label at initialisation:
$g_i \in \{\text{minority},\, \text{majority}\}$. The minority comprises
20% of all lay agents ($m = 0.2$). Group membership is permanent.

## 2.2 Extended Attractiveness Formula

The baseline attractiveness function is extended by a third
multiplicative term that captures behavioural ingroup preference:

$$\tilde{A}_{ij}(t) = \sigma\left(r_{op}(1-2|o_i-o_j|)\right) \cdot
  \sigma\left(r_{pw}\log\frac{p_j}{p_i}\right) \cdot
  \sigma\left(r_{ingroup} \cdot I(g_i=g_j)\right)$$

where the indicator

$$I(g_i = g_j) = \begin{cases} +1 & g_i = g_j \\ -1 & g_i \neq g_j \end{cases}$$

assigns a weight boost $\sigma(r_{ingroup}) > 0.5$ to ingroup pairs and
a discount $\sigma(-r_{ingroup}) < 0.5$ to outgroup pairs.

------------------------------------------------------------------------

# 3. Minority-Specific Metrics

Throughout this section, $N$ denotes the set of all agents,
$M \subseteq N$ the set of minority agents, $p_i$ the final voting power
of agent $i$, and $m = |M|/|N|$ the minority’s population share.

**Descriptive counts**

Before the normalized ratios below, it is useful to see the raw numbers
they are built from — the number of minority and majority votes cast, by
headcount and by power. Let $R \subseteq N$ denote the set of
representatives — the resolved agents who cast a vote directly, i.e. the
roots of the delegation forest (no outgoing delegation edge):

$$
r_M = \{i \in R : i \in M\},\qquad r_{N \setminus M} = \{i \in R : i \in N \setminus M\}
$$

$$
P_M = \sum_{i \in M} p_i, \qquad P_{N \setminus M} = \sum_{i \in N \setminus M} p_i
$$

$r_M$ and $r_{N \setminus M}$ are the number of minority and majority
representatives — the headcount analogue of $P_M$ and $P_{N \setminus
M}$, which are their raw total voting power. Read together, the two
reveal concentration directly: a small $r_{N \setminus M}$ paired with a
large $P_{N \setminus M}$ means minority voting power ends up
concentrated in the hands of very few majority delegates, whereas a
similar split in $r$ and $P$ would indicate a broad, undistorted mix of
representatives. $RR_m$ below is simply $P_M$ expressed as a share of
$P_M + P_{N \setminus M}$, relative to what the minority’s population
share $m = |M|/|N|$ would predict.

**Relative Representation**

$$
RR_m =
\frac{\displaystyle\sum_{i \in M} p_i \;/\; \sum_{i \in N} p_i}{m}
$$

- $RR_m = 1$: proportional representation.
- $RR_m < 1$: underrepresentation.
- $RR_m > 1$: overrepresentation.

**Minority Power Capture ($\text{PC}_m$)**

$$
\text{PC}_m =
\frac{\text{minority voting power represented by majority delegates}}
     {\text{total resolved minority voting power}}
$$

Fraction of resolved minority voting power controlled by a majority
delegate.

**Minority Representation Capture ($\text{RC}_m$)**

$$
\text{RC}_m =
\frac{\{i \in M : \text{root}(i) \in N \setminus M\}}
     {\{i \in M : \text{root}(i) \text{ is defined}\}}
$$

Fraction of resolved minority agents (by headcount) whose ultimate
delegate belongs to the majority; the headcount-weighted analogue of
$\text{PC}_m$ — the two measure the same thing, majority capture of
minority representation, just power- vs. headcount-weighted, which is
why they are named to match. $\text{RC}_m = 0$: no resolved minority
agent is represented by a majority delegate. $\text{RC}_m = 1$: every
resolved minority agent is.

**Minority Cross-group Delegation Rate ($\text{CDR}_{out}$)**

$$
\text{CDR}_{out} =
\frac{\#\{(i,j) \in E : i \in M,\, j \notin M\}}
     {\#\{(i,j) \in E : i \in M\}}
$$

where $E$ denotes the set of delegation edges formed in a given round.
Restricted to edges *originating from minority agents*: the share of the
minority’s own outgoing delegation edges that cross into the majority.

------------------------------------------------------------------------

# 4 Minority Representation - Objectives

## Objective 1: Minority Representation

Investigate how behavioural ingroup preference, opinion polarisation,
and network structure affect the political representation of minority
agents.

**Outcome measures**

- Relative Representation (RR_m)
- Minority Power Capture (PC_m)
- Minority Representation Capture (RC_m)

**Expected effects**

- Without explicit ingroup preference (r_ingroup = 0) under the `random`
  condition, minority representation is approximately proportional (RR_m
  ≈ 1): with no opinion or structural confound, there is nothing left to
  pull it away from parity.

- Increasing ingroup preference increases Relative Representation
  (RR_m).

- Increasing ingroup preference decreases Minority Power Capture (PC_m)
  and Minority Representation Capture (RC_m).

- The effects of ingroup preference on all three representation metrics
  are stronger under high polarisation (δ = 0.4) than under low
  polarisation (δ = 0.2), and stronger under `polarised` than under
  `random` at matched δ.

------------------------------------------------------------------------

## Objective 2: Delegation Mechanisms

Investigate the delegation behaviour underlying minority representation
outcomes and assess how behavioural ingroup preference influences
cross-group delegation.

**Outcome measures**

- Minority Cross-group Delegation Rate (CDR_out)
- Average delegation chain length
- Average ideological drift

These mechanism variables are analysed alongside the minority
representation metrics (RR_m, PC_m, RC_m) to explain why representation
changes across experimental conditions.

**Expected effects**

- Increasing ingroup preference decreases the Minority Cross-group
  Delegation Rate (CDR_out).

- The reduction in CDR_out is stronger under high polarisation than
  under low polarisation.

- Increasing ingroup preference decreases the average delegation chain
  length and the average ideological drift, for both minority and
  majority agents.
