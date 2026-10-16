Report 23 – Part II calibration (r_ig, homophily, minority share)
================
2026-09-20

# 1. Carried-over Part I calibration

- **N = 1000** lay agents (`n_per_community` = 1000, `n_communities` =
  1)
- **Network**: Watts-Strogatz small-world, node degree k = 60, rewiring
  p = 0.05
- **T = 80** rounds, of which **`n_v` = 40** are voting rounds (Delta t
  = 2, Sec. 1.4.1 trust update)
- **lambda = 0.129** (trust-relaxation adaptation rate, derived from a
  50% forget rate over 5 voting rounds)
- **c = 0.5** (`self_reliance_c`, Mobius self-vote curve)
- **`r_op` / `r_pw` / `r_tr` ceilings (100% of saturated effect) = 16384
  / 64 / 128** (full pct table in `Report_22` Sec. 6)

All carried over unchanged from Part 1 – adding the group-identity term
does not change the network, the temporal dynamics, or the trust
mechanism.

# 2. $r_{ig}$ calibration

$r_{ig}$ is swept in isolation under the *random* condition, holding
$r_{op}=r_{pw}=r_{tr}=0$, so the resulting saturation curve is
attributable to $r_{ig}$ alone.

![](Report_23_files/figure-gfm/p3-rig-plot-1.png)<!-- -->

**r_ig ceiling = 8.00** (read off on the Cross-group delegation rate
(CDR) curve).

RQ4/RQ5 use $r_{ig} \in \{0, 33, 66, 100\}$% throughout, translated via
the table below – the same convention as Part I’s
$r_{op}/r_{pw}/r_{tr}$.

| % of saturated effect |  r_ig |
|----------------------:|------:|
|                     0 | 0.000 |
|                    10 | 0.242 |
|                    20 | 0.485 |
|                    30 | 0.727 |
|                    40 | 0.969 |
|                    50 | 1.312 |
|                    60 | 1.669 |
|                    70 | 2.060 |
|                    80 | 2.849 |
|                    90 | 3.639 |
|                   100 | 8.000 |

r_ig needed to reach a given % of its own saturated effect

# 3. Homophily effects

Part II conditions:

- **random** – opinions uniform, independent of group; no homophily
  shuffle.
- **polarised** – minority opinions ~ U\[0, 0.5), majority ~ U\[0.5, 1);
  no homophily shuffle.
- **homophilic** – polarised opinions + group-homophily shuffle at
  `target_homophily`.
- **non-polarised-homophily** – random opinions + group-homophily
  shuffle at the same `target_homophily`.

Both homophily conditions cluster by **group label**
(`homophily_target = "group"`), and realised assortativity below is
measured on the group label as well.

**Maximum achievable homophily.** On the fixed network (Watts-Strogatz
ring, K = 60, 5% rewired edges) group assortativity has a structural
ceiling – the minority can at best form one contiguous block, whose
boundary and the randomly rewired edges always stay partly cross-group.
The level **max** therefore sets `target_homophily = 1` (unreachable),
so the shuffle uses its full step budget and the realised assortativity
is the empirical maximum for this network. It is compared against fixed
targets 0.3 / 0.4 / 0.5 / 0.6.

**Maximum achievable group assortativity: 0.768** (range 0.673–0.859
across 100 seeds).

![](Report_23_files/figure-gfm/p4-homophily-plot-1.png)<!-- -->

**Chosen: target_homophily = max** (realised group assortativity ≈
0.768).

# 4. Minority share

Across social-network and organisational-behaviour studies, minority
group sizes used for empirically motivated calibration typically fall in
roughly **15–40%** of the population, with shares below ~15% treated as
an extreme numerical imbalance rather than a representative “minority”
condition; 20% specifically is used as a minority size in network models
of homophily and minority representation . The sweep below spans the
full literature-supported range end to end, in 5-point steps
(**15/20/25/30/35/40%**), rather than just the two endpoints and the 20%
default.

Two background sweeps: (a) $r_{ig}$ alone, isolated
($r_{op}=r_{pw}=r_{tr}=0$), fixed at each of the four calibrated
percentages of its own saturated effect (Sec. 2) – same “one dimension
at a time” methodology as Sec. 2; (b) all four r’s shared
($r_{op}=r_{pw}=r_{tr}=r_{ig}$), each fixed at the same percentage of
its own saturated effect (Sec. 1/2, via `r_from_pct_shared()`) – same
convention as Sec. 3’s homophily background. (a) isolates $r_{ig}$’s
effect cleanly; (b) shows minority_share’s effect under a more
realistic, fully-engaged background.

![](Report_23_files/figure-gfm/p6-share-plot-1.png)<!-- -->

![](Report_23_files/figure-gfm/p6-share-bg-plot-1.png)<!-- -->

At N=1000 and minority share 20%, a run has ~200 minority agents –
comfortably above the point where per-run minority-side metrics ($RR$,
chain length/drift for the minority group) would be dominated by
small-sample noise. The sweep above shows delegation rate, lost-vote
rate, $RR_m$, and CDR behaving sensibly (no discontinuity or breakdown)
across the full 15–40% range and across all four `r_ig` levels, so
**20%** is kept as the fixed default used throughout Sec. 2/3.

# 5. Final calibrated values (Part II)

- **Minority share = 20%** – fixed design constant (Kanter, 1977),
  validated against Sec. 2/3 and the 15–40% sweep in Sec. 4 above, not a
  calibration target.
- **`target_homophily` = max** – maximum achievable group assortativity
  on this network (Sec. 3), realised 0.768 on average across 100 seeds.
- **`r_ig` ceiling = 8.00** – read off on the Cross-group delegation
  rate (CDR) curve (Sec. 2), isolated sweep with `r_op`=`r_pw`=`r_tr`=0.
- **K, T, `n_v`, Δt, λ, c** – carried over unchanged from `Report_22`
  Sec. 6 (see Sec. 1).
