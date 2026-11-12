# Liquid Democracy: an agent-based model of delegation

Code for the master's thesis on liquid democracy. Agents on a social network either vote themselves or delegate their vote to a neighbour, who may delegate it further. The model asks how three mechanisms shape the resulting delegation structure and its outcome:

- **opinion**: agents prefer neighbours with a similar opinion
- **power**: agents prefer neighbours who already hold many votes
- **trust**: agents learn from experience which neighbours represent them well

The final model and all results of the thesis are implemented in **Python** (package `liquid`). The model was first implemented in R; that earlier version is kept in `scripts/`, `reports/` and `simulations/`.

## Model

**World.** $N = 1000$ agents on a Watts-Strogatz network with $K = 30$ neighbours and 5 % rewiring. Every agent has a fixed opinion $o_i \in [0, 1]$ (uniform in Part I). The network and the opinions never change during a run. One round stands for one issue; a run has $T = 1000$ rounds.

**Attractiveness.** Each round, agent $i$ scores each neighbour $j$:

$$A_{i,j} = a^{op}_{i,j}\, a^{pw}_{i,j}\, a^{tr}_{i,j}$$

| factor | formula | effect |
|---|---|---|
| opinion | $a^{op}_{i,j} = e^{-r_{op} \lvert o_i - o_j \rvert}$ | lower for distant opinions |
| power | $a^{pw}_{i,j} = 2\, \mathcal{L}\big(r_{pw} (p_j / p_i - 1)\big)$ | higher for neighbours with more votes |
| trust | $a^{tr}_{i,j} = 2\, \mathcal{L}\big(r_{tr} (\tau_{i,j} - 1)\big)$ | higher for trusted neighbours |

$\mathcal{L}$ is the logistic function. With all responsiveness parameters $r$ at zero, every factor equals 1 and delegation is random.

**Decision.** With $A^{max}_i$ the attractiveness of the best neighbour, agent $i$ votes itself with probability

$$P_{self} = \frac{M - A^{max}_i}{M + \left(\frac{M - 1}{c} - M\right) A^{max}_i}, \qquad M = 4,$$

where $c = 0.5$ is the probability of voting directly when the best neighbour has $A = 1$. Otherwise it delegates to a neighbour drawn with probability proportional to $A_{i,j}$.

**Resolution.** Every delegation chain is followed to its root, the agent who votes directly, and the root's opinion is cast for everyone on the chain. Votes in a cycle, or in a chain that runs into one, are lost. Voting power is $p_i = 1 + \sum_{j \to i} p_j$, the number of votes that pass through agent $i$; neighbours see it in the next round.

**Trust.** After each round, an agent that delegated to $j$ updates its trust towards $j$:

$$\tau_{i,j} \leftarrow (1 - \lambda)\, \tau_{i,j} + \lambda\, (1 + s_{i,j}), \qquad \lambda = 1 - 2^{-1/5}.$$

The signal $s_{i,j}$ is $+1$ if the cast vote has the agent's own opinion, $0$ at distance $\theta$, $-1$ at the largest distance, and $-1$ if the vote was lost. $\theta$ is the expected distance between two random opinions of the population ($1/3$ for uniform opinions). Trust starts at the neutral value $1$, and there is no forgetting.

**Strength scale.** The responsiveness parameters are reported as a percentage of their effect after $T = 1000$ rounds (0 % = random delegation, 100 % = saturated). The calibration maps each percentage to a value of $r_{op}$, $r_{pw}$ and $r_{tr}$.

## Measures

| measure | definition |
|---|---|
| delegation rate | share of agents who delegate |
| lost-vote rate | share of agents whose vote ends in a cycle |
| chain length | average number of steps from a delegator to its root |
| ideological drift | average distance between a delegator's opinion and the vote cast for it |
| top-5 % power share | share of the cast votes held by the 5 % of agents who cast the most votes |
| 50 % share | smallest share of agents who together cast half of the votes |
| $W_1$ (Part II) | Wasserstein distance between the opinions of the population and the cast votes |
| Jensen-Shannon (Part II) | difference in the shape of the two distributions |

Part I reports the average over the last five rounds of each run; Part II uses the final round. All results use 100 seeds per parameter combination with 95 % confidence intervals across seeds.

## Python version

| folder / file | content |
|---|---|
| `liquid/` | the model: network, simulation, trust, metrics, batch runs with caching |
| `Part_1/calibration_part1.ipynb` | calibration: $K$, the strength scale, $c$, $\lambda$, $N$ |
| `Part_1/results_1.ipynb` | Part I: dynamics over time, the effect of each mechanism (RQ1 to RQ3), robustness on Barabási-Albert networks |
| `Part_2/part2.ipynb` | Part II: input against output ($W_1$, Jensen-Shannon, null model), mechanism combinations, referendum |
| `thesis_helpers.py`, `network_plots.py` | helpers for the notebooks |
| `tests/` | checks of the Python model against the R reference |

The simulation is written in JAX, so one round is computed for all agents at once. Simulation results are cached in `data/` and recomputed automatically when a parameter grid changes; figures and tables are written to `output/`.

**Requirements:** Python 3, `jax`, `numpy`, `pandas`, `scipy`, `matplotlib`, `python-igraph`, `jupyter`.

**Run** a notebook from the project folder, e.g.

```
python -m nbconvert --to notebook --execute --inplace Part_1/results_1.ipynb --ExecutePreprocessor.timeout=-1
```

Order after a recalibration: `calibration_part1` → `results_1` → `part2`. Both read the percentage-to-$r$ table from `output/calibration_part1/tables/5_final/pct_to_r.csv`.
