Weekly Report — Week 11 (01.05.2026 – 07.05.2026)
================
2026-05-05

## 1. Formulas

### Neighbour-Weight (Attractiveness)

$$A_{ij} = \sigma\!\left(r_{op} \cdot \bigl(1 -2\,|o_i - o_j|\bigr)\right) \cdot \sigma\!\left(r_{pw} \cdot \log\frac{p_j}{p_i}\right)$$

Defining the opinion similarity $s_{ij} = 1-2|o_i-o_j| \in [-1,1]$, this
is equivalent to
$A_{ij} = \sigma(-r_{op}\,s_{ij})\cdot\sigma(r_{pw}\log\tfrac{p_j}{p_i})$.

### Self-Weight

Let $j^\ast = \arg\max_{j \in N(i)} A_{ij}$ denote the most attractive
neighbour of agent $i$. The self-weight is then defined as

$$w_{\text{self}}(i)
= \sigma\!\left(-r_{op}\,s_{ij^\ast}\right)
\cdot \sigma\!\left(-r_{pw}\log\frac{p_i}{p_{j^\ast}}\right)$$

Equivalently,

$$w_{\text{self}}(i)
= \sigma\!\left(r_{op}\bigl(2\,|o_i - o_{j^\ast}| - 1\bigr)\right)
\cdot \sigma\!\left(r_{pw}\log\frac{p_{j^\ast}}{p_i}\right)$$

Thus, the self-weight is the mirrored counterpart of the
neighbour-weight: conditions that increase delegation attractiveness
reduce self-weight, and vice versa.

------------------------------------------------------------------------

## 2. Weight Surfaces (1-D Slices)

![](Report_11_files/figure-gfm/plot-1d-power-1.png)<!-- -->![](Report_11_files/figure-gfm/plot-1d-power-2.png)<!-- -->

## 3. Weight Surfaces (2-D Heatmaps)

![](Report_11_files/figure-gfm/plot-2d-heatmap-1.png)<!-- -->![](Report_11_files/figure-gfm/plot-2d-heatmap-2.png)<!-- -->

## 4. Simulation — Delegation Rate & Lost Vote Rate

The simulation runs `simulate_liquid_democracy` from `Network.R` using
the Neighbour-Weight and Self-Weight formulas across a $3\times3$ grid
of $r_{op},r_{pw} \in \{0,\,1,\,2\}$ ($n=250$ agents, $T=100$ rounds,
averaged over 100 independent runs).

![](Report_11_files/figure-gfm/simulation-1.png)<!-- -->

| r_op | r_pw | Del. mean | Del. SD | Lost mean | Lost SD |
|-----:|-----:|----------:|--------:|----------:|--------:|
|    0 |    0 |     0.856 |   0.002 |     0.511 |   0.008 |
|    0 |    1 |     0.832 |   0.003 |     0.457 |   0.011 |
|    0 |    2 |     0.819 |   0.005 |     0.430 |   0.013 |
|    1 |    0 |     0.918 |   0.002 |     0.688 |   0.022 |
|    1 |    1 |     0.903 |   0.004 |     0.641 |   0.027 |
|    1 |    2 |     0.893 |   0.005 |     0.614 |   0.030 |
|    2 |    0 |     0.957 |   0.002 |     0.828 |   0.013 |
|    2 |    1 |     0.951 |   0.002 |     0.807 |   0.015 |
|    2 |    2 |     0.948 |   0.002 |     0.794 |   0.017 |
