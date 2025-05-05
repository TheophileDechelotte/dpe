# Manipulations in French EPC ratings: investigating shopping behaviour

This repository accompanies my Master’s thesis on the EPC shopping behaviour in the French EPC scheme. It contains R pipelines to (i) run Monte‑Carlo simulations, (ii) construct prior and imprecision metrics, and (iii) estimate Regression‑Discontinuity (RD) effects on homeowners’ behaviour and retrofit outcomes. A reusable SQL view is provided to reproduce the cleaned micro‑dataset from the raw EPC database.

---

## File-by-file guide


### `monte-carlo.R`

|                     | Description |
|---------------------|-------------|
| **Purpose**         | Simulate repeated draws of EPC labels to simulate the theoretical EPC distribution. |
| **Main functions**  | `FD_binwidth()`, `Scott_binwidth()`, `compute_hist()`, `sample_from_hist()`, `simulate_group()`, `visualisation()`. |
| **Inputs**          | `alldpe_v2.csv` (raw EPC micro-data – _not in repo_). |
| **Outputs**         | `data/alldpe_simulation_scott.csv` (simulation panel); png plots under `graphs/`. |

### `metrics-simulation.R`

|                     | Description |
|---------------------|-------------|
| **Purpose**         | Compute two statistics per EPC subgroup – <br>`prior` (ex-ante probability of being energy efficient) and `epsilon` (imprecision of the EPC draw) – following the Scott/FD bin-width methods. |
| **Main functions**  | `compute_prior()`, `compute_epsilon()`, `get_subgroup_metrics()`, `plot_prior_eps_box()`. |
| **Inputs**          | Same raw EPC file as above plus `data/alldpe_simulation_scott.csv`. |
| **Outputs**         | `data/alldpe_metrics_scott_v3.csv` (metrics) ; `graphs/Boxplot_Prior_Epsilon_<band>.png`. |

### `RD-incentives-330.R`

|                     | Description |
|---------------------|-------------|
| **Purpose**         | Sharp RD estimation of the probability that a homeowner shop for a second EPC to move just below the 330 kWh/m² cut-off (indicator `shopping`). |
| **Inputs**          | `data/alldpe_metrics_scott_v3.csv`. |
| **Outputs**         | Multiple visuals saved to `graphs/` (RD plot, donut RD, covariate balance, heterogeneity). |

### `RD-outcome-330.R`

|                     | Description |
|---------------------|-------------|
| **Purpose**         | RD estimation of retrofit effort: compares pre- vs post-inspection behaviour and nets out shopping bias. |
| **Inputs**          | Same metrics file (`alldpe_metrics_scott_v3.csv`). |
| **Outputs**         | RD and donut plots for pre/post shopping & DID RD effect. |

### `view.sql`

|                     | Description |
|---------------------|-------------|
| **Purpose**            | Defines a PostgreSQL view `alldpe_v2` that: <br>• joins the EPC core table with prior/next inspections ; <br>• computes time-interval variables (`interval_dpe_remplacant`, `interval_ancien_dpe`) ; <br>• normalises consumption per m² and splits it by use (heating, DHW, etc.). |

---

## Reproducibility notes

* **R version:** ≥ 4.3 recommended.  
* **Database:** The SQL view assumes PostgreSQL ≥ 14 with the PostGIS extension for potential spatial joins.  
* **Randomness:** Set seed at the top of each script (`set.seed(123)`) for deterministic Monte-Carlo draws.  
* **Heavy computations:** The MC simulation may take > 1 h on a laptop.

