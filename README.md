# dpe

File-by-file Guide

File	Purpose	Key inputs	Main outputs / side-effects	Notable functions
R/7-monte-carlo.R	Generates a Monte-Carlo replica of the empirical density of energy consumption; computes two diagnostic metrics—π̂ (prior probability of being ≤ 330) and ε̂ (ex-ante imprecision); tests whether measurement error alone can explain any observed bunching.	data/alldpe_v2.csvdata/alldpe_simulation_Scott.csv	• graphs/monte-carlo-*.png (density comparison, π/ε box-plots)• Data frame printed to console	FD_binwidth(), Scott_binwidth() – optimal bin widths;simulate_group() – draws synthetic sample for a subgroup;compute_prior(), compute_epsilon() – diagnostic metrics
R/7-RD-incentives-330.R	Baseline RD estimating the effect of the threshold on “shopping” behaviour (commissioning a new DPE). Also runs placebo RDs, covariate balance checks and heterogeneity analyses.	data/alldpe_metrics_Scott_v3.csv	• graphs/RD-shopping-estimate.png• graphs/heterogeneous_RD_prior.png• graphs/heterogeneous_RD_imprecision.png	— (analysis is piped); relies on rdrobust(), rddensity()
R/7-RD-outcome-330.R	RD for outcome variables (post-shopping rating, diff-in-RD), again with donut-RD, covariate balance and subgroup robustness.	data/alldpe_metrics_Scott_v3.csv	• graphs/RD-post-shopping-estimate-donut.png• graphs/RD-diff-shopping-estimate-donut.png	— (analysis is piped)
sql/view.sql	Builds a PostgreSQL view that filters implausible records, merges DPE tables, computes consumption per usage and key timing intervals (interval_dpe_remplacant, interval_ancien_dpe) used by the R scripts.	Raw DPE tables in the same database (diagnostic_*, batiment_*, …)	A view named dpe_clean_metrics_v3 (or similar—see last line of file)	Pure SQL
