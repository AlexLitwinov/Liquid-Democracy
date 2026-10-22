# Report 24 -- Part I results, mechanisms and trust


Settings: N = 1000, K = 70 (Watts-Strogatz), T = 80, 40 voting rounds, 25 seeds per point.

# RQ1: Delegation dynamics

![](Report_24_files/results_part1_1_RQ1_delegation_all.png)

## Delegation cycles

![](Report_24_files/results_part1_4_Cycles_all_cycles.png)

Metrics (last 30 rounds of each run):

- **Cycles per round**: number of delegation cycles
- **Share of 2-cycles**: cycles with two members (i -> j, j -> i)
- **Feeding in (share)**: agents whose chain ends in a cycle without being part of it
- **Opinion-dist. ratio (2-cycles)**:

  $$\text{ratio} = \frac{\text{mean } |o_i - o_j| \text{ within 2-cycles}}{\text{mean } |o_i - o_{t(i)}| \text{ over all delegations } i \to t(i)}$$

  1 = members of a 2-cycle are as far apart as a typical delegation pair, < 1 = closer

Short:

- Opinion: almost only 2-cycles between similar agents (ratio ~0.6 falling to ~0.35), hardly anyone feeds in
- Power and trust: larger cycles, ratio ~1, most lost votes from agents feeding in

## Explanation: power

![](Report_24_files/results_part1_5_Mechanisms_pw_only_explanation.png)

Metrics:

- **Downhill step**: delegation to a neighbour with less voting power than the delegator (power of the previous round)
- **Chains with / without downhill step**: share of resolved chains containing at least one / no downhill step
- **Chain length with / no downhill step**: average length of these two kinds of chains

Short:

- Long chains needa downhill (or equal-power) step
- pw only: downhill chains vanish between 70 and 100 % -> cycles and lost votes collapse
- Long chains are the downhill ones (~4-4.4 steps) -> their disappearance causes the chain-length kink

# RQ2: Quality of representation

![](Report_24_files/results_part1_2_RQ2_representation_drift_delegators.png)

Metric:

- **Avg. ideological drift (delegators)**: mean |o_i - o_R(i)| between a delegator's opinion and the vote cast for it (lost votes excluded)
- Random benchmark: a delegator with a random representative has drift 1/3

Short:

- Opinion lowers drift right away: 0.33 -> 0.11 at 10 %, 0.04 at 100 %
- Power and trust stay at the random benchmark (~0.33): they change how many delegate, not how well they choose
- Combined: as soon as r_op > 0, drift stays low (~0.04-0.06) whatever r_pw and r_tr

# Understanding trust

Question: trust should let agents learn which neighbours represent them well, so delegator drift should fall below the random value 1/3. In RQ2 it does not. This section checks whether the trust signal or the choice rule is the reason.

**Trust signal.** After a voting round, every delegator $i$ compares its own opinion $o_i$ with the vote $v$ that was cast for it (the opinion of its chain's root), $d = |o_i - v|$:

$$s = \max\left(1 - \frac{d}{\theta},\ -1\right)$$

- $s = 1$ if the vote matches exactly ($d = 0$), $s = 0$ at $d = \theta$, negative beyond, never below $-1$ (lost vote: $s = -1$)
- $s$ only updates trust in the chosen neighbour: $\tau \leftarrow (1 - \lambda)\,\tau + \lambda\,(1 + s)$, so $s > 0$ pushes trust above the neutral 1 and $s < 0$ below it
- $\theta$ is the **neutral distance**: votes closer than $\theta$ are good news, votes farther away bad news

**Where 1/3 comes from.** Opinions are uniform on $[0, 1]$ and independent of the network, so a neighbour picked at random is at expected distance

$$E\,|o_i - o_j| = \int_0^1\!\!\int_0^1 |x - y|\,dx\,dy = \frac{1}{3}$$

1/3 is therefore the drift of a delegator who learns nothing. A useful signal should be about 0 at that distance, positive only for better-than-random delegates.

| | baseline $\theta = 0.5$ | $\theta = 1/3$ |
|---|---|---|
| signal | $s = 1 - 2d$ | $s = \max(1 - 3d,\ -1)$ |
| neutral at | $d = 0.5$ | $d = 1/3$ (random) |
| $s$ at random distance $d = 1/3$ | $+1/3$ | $0$ |
| mean $s$ for a random delegate | $+0.33$ | $+0.04$ |
| example $d = 0.1 / 0.3 / 0.5$ | $0.8 / 0.4 / 0$ | $0.7 / 0.1 / -0.5$ |

- With $\theta = 0.5$, even a random delegate gets positive signals on average: trust rises for whoever is chosen, close or not, so it reinforces habit rather than quality
- With $\theta = 1/3$, only delegates closer than random gain trust, worse ones lose it: the signal separates good from bad delegates

Variants (trust only, r_op = r_pw = 0):

| variant | change |
|---|---|
| baseline (theta = 0.5) | model as in the thesis |
| theta = 1/3 | only delegates closer than a random neighbour (1/3) are rewarded |
| slow forgetting (50 % / 20 VR) | neighbours not delegated to forget 50 % over 20 instead of 5 voting rounds |
| no forgetting | neighbours not delegated to keep their trust |

Metrics:

- **Avg. ideological drift (delegators)**: drift among delegating agents only, 1/3 = random choice
- **Most chosen neighbour**: per agent, how often it chose its most chosen neighbour over the run (= trust signals to that neighbour)
- **Mean trust by distance**: final trust tau toward neighbours with at least one signal, by opinion distance |o_i - o_j|

## K = 70, T = 80 (40 voting rounds)

![](Report_24_files/trust_variants_K70_outcomes.png)

![](Report_24_files/trust_variants_K70_signals_most_chosen.png)

![](Report_24_files/trust_variants_K70_tau_by_distance.png)

- Delegator drift stays at ~0.33 for every variant
- Most chosen neighbour: only ~2-3 of ~25 delegations
- Trust hardly falls with distance (tau ~1.01 close vs ~1.00 far)

## K = 70, T = 500 (500 voting rounds)

![](Report_24_files/trust_variants_K70_T500_outcomes.png)

![](Report_24_files/trust_variants_K70_T500_tau_by_distance.png)

- More signals barely help: only "no forgetting" lowers delegator drift (0.30 at 100 %)

## K = 10, T = 80 (40 voting rounds)

![](Report_24_files/trust_variants_K10_outcomes.png)

![](Report_24_files/trust_variants_K10_signals_most_chosen.png)

![](Report_24_files/trust_variants_K10_tau_by_distance.png)

- Fewer neighbours: most chosen neighbour gets ~5-6 delegations
- Learning becomes visible with no / slow forgetting (drift 0.29 / 0.30 at 100 %)

## K = 70, delegate to the best neighbour

A delegating agent always picks its most attractive neighbour instead of drawing one in proportion to A (ties broken at random).

![](Report_24_files/trust_variants_K70_greedy_outcomes.png)

![](Report_24_files/trust_variants_K70_greedy_tau_by_distance.png)

- Delegator drift drops to ~0.23-0.25, with theta = 1/3 to ~0.18-0.21
- -> the bottleneck is the proportional choice among 70 neighbours (trust factor capped at 2), not the trust signal

# RQ3: Concentration of voting power

![](Report_24_files/results_part1_3_RQ3_concentration_all_ws_vs_ba.png)

Metrics:

- **Top-5 % voting-power share**: share of the cast votes held by the 5 % most powerful agents
- **Agents needed for 50 % of power**: smallest share of agents that together cast half of the votes

Short:

- Solid = Watts-Strogatz, dashed = Barabasi-Albert (same N, K, r values; % scale calibrated on WS)
- BA concentrates power more under power responsiveness (top-5 % at 20 %: 0.54 vs 0.43, at saturation 0.75 vs 0.69); opinion and trust differ little

# Report 23: Part II calibration (r_ig, homophily, minority share)

Python version of `Report_23.Rmd`. Every result is saved twice under the same name:

```
output/report23/
  figures/<folder>/<name>.png
  tables/<folder>/<name>.csv      values behind the figure: mean, 95% CI and number of seeds per point
  timings.csv                     how long every step took
```

| folder | content |
|---|---|
| `1_Part1_carryover` | settings and the % -> r table carried over from Part I |
| `2_r_ig_calibration` | isolated r_ig sweep, r_ig ceiling and % -> r_ig table |
| `3_Homophily` | homophily levels 0.3-0.6 vs. maximum, across background levels |
| `4_Minority_share` | minority share 15-40 %, r_ig alone and all four r together |
| `5_Final_values` | final Part II settings |

## 1. Carried-over Part I calibration




<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>setting</th>
      <th>value</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>N</td>
      <td>1000</td>
    </tr>
    <tr>
      <th>1</th>
      <td>network</td>
      <td>ws</td>
    </tr>
    <tr>
      <th>2</th>
      <td>K</td>
      <td>60</td>
    </tr>
    <tr>
      <th>3</th>
      <td>p_rewire</td>
      <td>0.05</td>
    </tr>
    <tr>
      <th>4</th>
      <td>T</td>
      <td>80</td>
    </tr>
    <tr>
      <th>5</th>
      <td>n_voting_rounds</td>
      <td>40</td>
    </tr>
    <tr>
      <th>6</th>
      <td>lambda</td>
      <td>0.1294</td>
    </tr>
    <tr>
      <th>7</th>
      <td>c</td>
      <td>0.5</td>
    </tr>
    <tr>
      <th>8</th>
      <td>minority share</td>
      <td>0.2</td>
    </tr>
    <tr>
      <th>9</th>
      <td>seeds</td>
      <td>100</td>
    </tr>
  </tbody>
</table>
</div>



## 2. r_ig calibration

r_ig swept alone (random condition, r_op = r_pw = r_tr = 0).


    
![png](Report_24_files/Report_24_16_0.png)
    





<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>r_ig</th>
      <th>metric</th>
      <th>mean_val</th>
      <th>ci_lo</th>
      <th>ci_hi</th>
      <th>n_seeds</th>
      <th>group</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>Delegation rate</td>
      <td>0.500466</td>
      <td>0.499051</td>
      <td>0.501881</td>
      <td>100.0</td>
      <td>Pooled</td>
    </tr>
    <tr>
      <th>1</th>
      <td>0</td>
      <td>Delegation stability</td>
      <td>0.253968</td>
      <td>0.252552</td>
      <td>0.255384</td>
      <td>100.0</td>
      <td>Pooled</td>
    </tr>
    <tr>
      <th>2</th>
      <td>0</td>
      <td>Lost-vote rate</td>
      <td>0.012072</td>
      <td>0.011363</td>
      <td>0.012781</td>
      <td>100.0</td>
      <td>Pooled</td>
    </tr>
    <tr>
      <th>3</th>
      <td>0</td>
      <td>Avg. chain length</td>
      <td>1.936795</td>
      <td>1.924793</td>
      <td>1.948796</td>
      <td>100.0</td>
      <td>Pooled</td>
    </tr>
    <tr>
      <th>4</th>
      <td>0</td>
      <td>Top-5% voting-power share</td>
      <td>0.328288</td>
      <td>0.326456</td>
      <td>0.330120</td>
      <td>100.0</td>
      <td>Pooled</td>
    </tr>
    <tr>
      <th>...</th>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
      <td>...</td>
    </tr>
    <tr>
      <th>267</th>
      <td>16384</td>
      <td>Cross-group delegation rate (CDR)</td>
      <td>0.000000</td>
      <td>0.000000</td>
      <td>0.000000</td>
      <td>100.0</td>
      <td>Majority</td>
    </tr>
    <tr>
      <th>268</th>
      <td>32768</td>
      <td>Delegation rate</td>
      <td>0.500298</td>
      <td>0.498716</td>
      <td>0.501879</td>
      <td>100.0</td>
      <td>Majority</td>
    </tr>
    <tr>
      <th>269</th>
      <td>32768</td>
      <td>Lost-vote rate</td>
      <td>0.014515</td>
      <td>0.013575</td>
      <td>0.015455</td>
      <td>100.0</td>
      <td>Majority</td>
    </tr>
    <tr>
      <th>270</th>
      <td>32768</td>
      <td>Avg. chain length</td>
      <td>1.925854</td>
      <td>1.912836</td>
      <td>1.938871</td>
      <td>100.0</td>
      <td>Majority</td>
    </tr>
    <tr>
      <th>271</th>
      <td>32768</td>
      <td>Cross-group delegation rate (CDR)</td>
      <td>0.000000</td>
      <td>0.000000</td>
      <td>0.000000</td>
      <td>100.0</td>
      <td>Majority</td>
    </tr>
  </tbody>
</table>
<p>272 rows × 7 columns</p>
</div>



Ceiling read off the plot: CDR has flattened out by r_ig = 8 (delegation rate is flat over the whole range, so it cannot anchor the % scale).

**Why the delegation rate stays at 0.5**

- Vote or delegate depends only on the best neighbour: every agent has same-group neighbours (at least 3) with $A = 1$, so $P_{self} = c = 0.5$ for every r_ig
- r_ig only changes *whom* an agent delegates to: CDR falls from 0.32 to 0

**Why the minority's chain length falls**

- Minority delegations stay among its ~12 same-group neighbours, so mutual delegations and cycles become likely: minority lost votes 1.2 % -> 5.4 %
- Long chains are the ones most likely to end in a cycle, so the remaining resolved chains are shorter (1.93 -> 1.74)


<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>pct</th>
      <th>r_ig</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>0.000</td>
    </tr>
    <tr>
      <th>1</th>
      <td>10</td>
      <td>0.245</td>
    </tr>
    <tr>
      <th>2</th>
      <td>20</td>
      <td>0.489</td>
    </tr>
    <tr>
      <th>3</th>
      <td>30</td>
      <td>0.734</td>
    </tr>
    <tr>
      <th>4</th>
      <td>40</td>
      <td>0.978</td>
    </tr>
    <tr>
      <th>5</th>
      <td>50</td>
      <td>1.328</td>
    </tr>
    <tr>
      <th>6</th>
      <td>60</td>
      <td>1.687</td>
    </tr>
    <tr>
      <th>7</th>
      <td>70</td>
      <td>2.102</td>
    </tr>
    <tr>
      <th>8</th>
      <td>80</td>
      <td>2.881</td>
    </tr>
    <tr>
      <th>9</th>
      <td>90</td>
      <td>3.661</td>
    </tr>
    <tr>
      <th>10</th>
      <td>100</td>
      <td>8.000</td>
    </tr>
  </tbody>
</table>
</div>


## 3. Homophily effects

| condition | opinions | network |
|---|---|---|
| random | uniform, independent of group | no shuffle |
| polarised | minority ~ U[0, 0.5), majority ~ U[0.5, 1) | no shuffle |
| homophilic | polarised | group-homophily shuffle |
| non-polarised-homophily | uniform | group-homophily shuffle |

**Group-homophily shuffle** (`liquid/groups.py`; R: `shuffle_group_homophily`)

The network topology remains fixed, and 20 % of agents are randomly assigned to the minority group. The algorithm repeatedly picks one minority agent $i$ and one majority agent $j$ that each have at least one neighbour from the other group, and proposes to swap them: $i$ and $j$ trade places, taking their group membership and opinion with them, so the edges stay the same but different agents sit on them.

The swap is scored by the change in the number of cross-group edges $E$ (edges between a minority and a majority agent):

$$\Delta E = E_{\mathrm{after}} - E_{\mathrm{before}}$$

- $\Delta E < 0$: the swap separates the groups more
- $\Delta E > 0$: the swap mixes them more

A proposed swap is accepted with probability

$$P(\mathrm{accept}) = \min\left(1,\ e^{-\Delta E\, t(s)}\right), \qquad t(s) = 0.5 \cdot 10^{\,s/S},$$

where $s$ is the current step and $S = 400\,000$ is the step budget.

- $\Delta E \le 0$: $e^{-\Delta E\, t} \ge 1$, so the swap is always accepted
- $\Delta E > 0$: accepted only by chance, with probability $e^{-\Delta E\, t}$
- $t(s)$ grows from 0.5 at the start to 5 at the end (factor 10 over the run), so **the later the step, the stricter the decision**: for $\Delta E = +1$ the acceptance probability is $e^{-0.5} \approx 0.61$ at the start, $e^{-1.58} \approx 0.21$ halfway and $e^{-5} \approx 0.007$ at the end
- Early on, swaps that briefly mix the groups are still allowed, which helps the search escape local dead ends; later almost only improving swaps get through (simulated annealing)

The algorithm stops when the target assortativity is reached (checked every 100 steps) or when the step budget is used up. The target **max** is set to 1, i.e. complete group separation. This may be unattainable for a given network, and the finite search does not guarantee that the highest attainable assortativity is found.

**Group assortativity** (`liquid/groups.py`; `igraph::assortativity` on the binary minority label): the correlation of the group label at the two ends of an edge. Let $x_i = 1$ for minority and $x_i = 0$ for majority agents, and count every edge from both ends. Then

$$r = \frac{\overline{x_u x_v} - \bar{x}^{\,2}}{\bar{x}\,(1 - \bar{x})}$$

- $\bar{x}$: share of edge ends at minority agents (≈ 0.2 at 20 % minority with similar degrees)
- $\overline{x_u x_v}$: share of edge ends whose edge connects two minority agents
- $\bar{x}^{\,2}$: what $\overline{x_u x_v}$ would be under random mixing, so the numerator measures the excess of minority-minority edges over chance
- $\bar{x}(1 - \bar{x})$: the variance of the label, scales $r$ so that complete separation gives exactly 1

Example with $\bar{x} = 0.2$: random mixing gives $\overline{x_u x_v} = 0.04$ and $r = 0$; complete separation gives $\overline{x_u x_v} = 0.2$ and $r = (0.2 - 0.04)/(0.2 \cdot 0.8) = 1$. Negative values mean fewer same-group edges than by chance.

In short: the shuffle reduces cross-group edges through increasingly strict probabilistic swaps, and $r$ measures how much more often agents are linked to their own group than chance would predict.


    
![png](Report_24_files/Report_24_20_0.png)
    



<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>mean</th>
      <th>min</th>
      <th>max</th>
      <th>seeds</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0.788</td>
      <td>0.673</td>
      <td>0.859</td>
      <td>100</td>
    </tr>
  </tbody>
</table>
</div>


Chosen for RQ4/RQ5: **target homophily = max** (as long as CDR has not collapsed to 0 there).

## 4. Minority share

Sweep over 15-40 % in 5-point steps: (a) r_ig alone at 0/33/66/100 % of its saturated effect, r_op = r_pw = r_tr = 0; (b) all four r at the same %. Random condition.


    
![png](Report_24_files/Report_24_24_0.png)
    



    
![png](Report_24_files/Report_24_24_1.png)
    


Delegation rate, lost votes, RR_m and CDR behave smoothly over 15-40 %, so the minority share stays at **20 %** (about 200 minority agents at N = 1000).

## 5. Final calibrated values (Part II)


<div>
<style scoped>
    .dataframe tbody tr th:only-of-type {
        vertical-align: middle;
    }

    .dataframe tbody tr th {
        vertical-align: top;
    }

    .dataframe thead th {
        text-align: right;
    }
</style>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>setting</th>
      <th>value</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>minority share</td>
      <td>0.2</td>
    </tr>
    <tr>
      <th>1</th>
      <td>target homophily</td>
      <td>max</td>
    </tr>
    <tr>
      <th>2</th>
      <td>realised assortativity at max (mean over seeds)</td>
      <td>0.788</td>
    </tr>
    <tr>
      <th>3</th>
      <td>r_ig ceiling</td>
      <td>8</td>
    </tr>
    <tr>
      <th>4</th>
      <td>r_ig ceiling read off on</td>
      <td>Cross-group delegation rate (CDR)</td>
    </tr>
    <tr>
      <th>5</th>
      <td>K, T, n_v, lambda, c</td>
      <td>carried over from Part I (Sec. 1)</td>
    </tr>
  </tbody>
</table>
</div>

