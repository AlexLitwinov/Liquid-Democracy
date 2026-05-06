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

$$j^* = \arg\max_{j \in N(i)} A_{ij}, \qquad w_{\text{self}}(i) = \sigma\!\left(r_{op} \cdot \bigl(1 - 2\,|o_i - o_{j^*}|\bigr)\right) \cdot \sigma\!\left(r_{pw} \cdot \log\frac{p_{j^*}}{p_i}\right)$$

Equivalently,
$w_{\text{self}}(i) = \sigma(r_{op}\,s_{ij^*})\cdot\sigma(r_{pw}\log\tfrac{p_{j^*}}{p_i})$.
agent’s own position.

------------------------------------------------------------------------

## 2. Weight Surfaces (1-D Slices)

![](Report_11_files/figure-gfm/plot-1d-power-1.png)<!-- -->

## 3. Weight Surfaces (2-D Heatmaps)

![](Report_11_files/figure-gfm/plot-2d-heatmap-1.png)<!-- -->

## 4. Simulation — Delegation Rate & Lost Vote Rate

The simulation runs `simulate_liquid_democracy` from `Network.R` using
the Neighbour-Weight and Self-Weight formulas across a $3\times3$ grid
of $r_{op},r_{pw} \in \{0.5,\,1,\,2\}$ ($n=250$ agents, $T=100$ rounds,
averaged over 100 independent runs).

![](Report_11_files/figure-gfm/simulation-1.png)<!-- -->

| r_op | r_pw | Del. mean | Del. SD | Lost mean | Lost SD |
|-----:|-----:|----------:|--------:|----------:|--------:|
|  0.5 |  0.5 |     0.834 |   0.003 |     0.460 |   0.009 |
|  0.5 |  1.0 |     0.824 |   0.003 |     0.441 |   0.009 |
|  0.5 |  2.0 |     0.812 |   0.004 |     0.416 |   0.007 |
|  1.0 |  0.5 |     0.825 |   0.002 |     0.444 |   0.009 |
|  1.0 |  1.0 |     0.817 |   0.003 |     0.428 |   0.008 |
|  1.0 |  2.0 |     0.805 |   0.004 |     0.404 |   0.009 |
|  2.0 |  0.5 |     0.812 |   0.003 |     0.425 |   0.007 |
|  2.0 |  1.0 |     0.804 |   0.003 |     0.411 |   0.008 |
|  2.0 |  2.0 |     0.793 |   0.004 |     0.389 |   0.008 |
