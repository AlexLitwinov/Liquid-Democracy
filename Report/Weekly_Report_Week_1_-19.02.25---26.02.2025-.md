Report Week 1 (19.02.25 - 26.02.2025)
================
2026-02-26

- [Summary](#summary)
- [Work Done](#work-done)
- [Open Issues](#open-issues)

## Summary

First week of implementation. The existing NetLogo model was translated
into R and a first working simulation was established.

## Work Done

**Model Translation (NetLogo → R)**

- Rebuilt core simulation logic in R using `igraph` and `tibble`
- Implemented agent setup with lay/expert types, community assignment,
  and fixed preference values on \[0,1\]
- Implemented ring-lattice friendship network with expert connectivity

**First Summary & Visualisation Functions**

- Written initial `summary_metrics()` covering lost vote rate, mean/max
  power, and basic delegation structure
- Written first `plot_social_and_delegation()` for network visualisation

## Open Issues

- Rewiring not yet correctly implemented
- Power computation returning 1 for all agents (cycle bug not yet
  identified)
