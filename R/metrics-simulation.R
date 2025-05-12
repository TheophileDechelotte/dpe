library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)

# 0. Chargement des données ----

df <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_v2.csv")

df <- df %>%
  mutate(
    type_energie_chauffage = case_when(
      chauffage_principal %in% c("Bois – Bûches", "Bois – Plaquettes forestières",
                                 "Bois – Plaquettes d’industrie",
                                 "Bois – Granulés (pellets) ou briquettes") ~ "Bois",
      chauffage_principal == "Fioul domestique" ~ "Fioul",
      chauffage_principal %in% c("Gaz naturel", "GPL", "Propane","Butane") ~ "Gaz",
      chauffage_principal %in% c("Réseau de Chauffage urbain", "Réseau de Froid Urbain") ~ "Reseau chaleur",
      chauffage_principal %in% c("Électricité", 
                                 "Électricité d'origine renouvelable utilisée dans le bâtiment") ~ "Electricite",
      chauffage_principal == "Charbon" ~ "Charbon"),
    is_ancien_dpe = !is.na(dpe_remplacant_id),
    is_dpe_remplacant = !is.na(ancien_dpe_id),
    type_logement = as.factor(type_logement),
    periode_construction = as.factor(periode_construction),
    type_energie_chauffage = as.factor(type_energie_chauffage))

df_filtered <- df %>%
  filter(!is_dpe_remplacant)

dpe_classes <- c(0, 70, 110, 180, 250, 330, 420, 800)
dpe_thresholds <- c(70, 110, 180, 250, 330, 420)
dpe_classes_colors <- c("darkgreen", "forestgreen","green", "gold", "orange", "darkorange", "red")

simulation_results_CSV <- read_csv("C:\\Users\\tdechelotte\\Desktop\\group_simulation_scott.csv")

# 1. Fonctions utilitaires ----

# Compute prior: share of simulated observations in [250,420) below a given threshold.
compute_prior <- function(sim_data,
                          lower = 250,
                          upper = 420,
                          threshold = 330) {
  sim_window <- sim_data %>%
    filter(total >= lower, total < upper)

  if (nrow(sim_window) == 0) return(NA_real_)

  mean(sim_window$total < threshold)
}

# Compute epsilon: area where real density > sim density over [range_lower, range_upper].
compute_epsilon <- function(real_data,
                            sim_data,
                            lower      = 250,
                            upper      = 420,
                            threshold  = 330,
                            binwidth   = 1) {

  # 0. Keep only the window we’re analysing -------------------------------
  sim_window  <- sim_data  %>% filter(total                 >= lower,
                                      total                 <  upper)
  real_window <- real_data %>% filter(ep_conso_5_usages_m2  >= lower,
                                      ep_conso_5_usages_m2  <  upper)

  if (nrow(sim_window) == 0 || nrow(real_window) == 0)
    return(NA_real_)

  # 1. Histograms on *identical* breaks ----------------------------------
  breaks_seq <- seq(lower, upper, by = binwidth)

  h_sim  <- hist(sim_window$total,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)            # density integrates to 1
  h_real <- hist(real_window$ep_conso_5_usages_m2,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)

  dens_sim   <- h_sim$density
  dens_real  <- h_real$density
  mids       <- h_real$mids              # common mid-points

  # 2. Positive density gap (observed – baseline) ------------------------
  diff_pos <- pmax(dens_real - dens_sim, 0)

  # 3. “Excess” mass just *before* and *after* the boundary --------------
  excess_before <- sum(diff_pos[mids <  threshold]) * binwidth

  # 4. Total observed mass in the class right *after* the boundary -------
  total_after <- sum(dens_real[mids >= threshold]) * binwidth
  
  denom <- excess_before + total_after
  if (denom == 0) return(NA_real_)

  epsilon <- excess_before / denom        # 0–1; ×100 for %
  return(epsilon)
}

compute_epsilon2 <- function(real_data,
                            sim_data,
                            lower      = 250,
                            upper      = 420,
                            threshold  = 330,
                            binwidth   = 1) {

  # 0. Keep only the window we’re analysing -------------------------------
  sim_window  <- sim_data  %>% filter(total                 >= lower,
                                      total                 <  upper)
  real_window <- real_data %>% filter(ep_conso_5_usages_m2  >= lower,
                                      ep_conso_5_usages_m2  <  upper)

  if (nrow(sim_window) == 0 || nrow(real_window) == 0)
    return(NA_real_)

  # 1. Histograms on *identical* breaks ----------------------------------
  breaks_seq <- seq(lower, upper, by = binwidth)

  h_sim  <- hist(sim_window$total,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)            # density integrates to 1
  h_real <- hist(real_window$ep_conso_5_usages_m2,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)

  dens_sim   <- h_sim$density
  dens_real  <- h_real$density
  mids       <- h_real$mids              # common mid-points

  # 2. Positive density gap (observed – baseline) ------------------------
  diff_pos <- pmax(dens_real - dens_sim, 0)

  # 3. “Excess” mass just *before* and *after* the boundary --------------
  excess_before <- sum(diff_pos[mids <  threshold]) * binwidth
  excess_after <- sum(diff_pos[mids >=  threshold]) * binwidth

  # 4. Total observed mass in the class right *after* the boundary -------
  total_after <- sum(dens_real[mids >= threshold]) * binwidth
  
  denom <- excess_before + total_after - excess_after
  if (denom == 0) return(NA_real_)

  epsilon <- excess_before / denom        # 0–1; ×100 for %
  return(epsilon)
}

# Compute π and ε for every subgroup in one shot
get_subgroup_metrics <- function(sim_df,
                                 real_df,
                                 lower        = 250,
                                 upper        = 420,
                                 threshold    = 330,
                                 binwidth     = 1) {
 
  ## Build column names
  prior_col   <- paste0("prior_",   threshold)
  epsilon_col <- paste0("epsilon_", threshold)
  
  subgroups_tbl <- sim_df %>%
    distinct(type_logement, periode_construction, type_energie_chauffage)

  subgroups_tbl %>%
    mutate(
      metrics = pmap(
        list(type_logement, periode_construction, type_energie_chauffage),
        function(tl, pc, tec) {
 
          # Slice simulation & real data for the subgroup
          sim_sub  <- sim_df  %>% filter(type_logement == tl,
                                         periode_construction == pc,
                                         type_energie_chauffage == tec)
 
          real_sub <- real_df %>% filter(type_logement == tl,
                                         periode_construction == pc,
                                         type_energie_chauffage == tec)
 
          # π and ε (or NA if nothing to compute)
          if (nrow(sim_sub) == 0 || nrow(real_sub) == 0) {
            tibble(!!prior_col   := NA_real_,
                   !!epsilon_col := NA_real_)
          } else {
            pi_val <- compute_prior(sim_sub,
                                    lower     = lower,
                                    upper     = upper,
                                    threshold = threshold)
 
            eps_val <- compute_epsilon(real_sub, sim_sub,
                                       lower     = lower,
                                       upper     = upper,
                                       threshold = threshold,
                                       binwidth  = binwidth)
  
            tibble(!!prior_col   := pi_val,
                   !!epsilon_col := eps_val)
          }
        }
      )
    ) %>%
    unnest(cols = metrics)
}

# One box‑plot for prior & epsilon in a metrics tibble 
plot_prior_eps_box <- function(metrics_df, win_tag) {
  metrics_long <- metrics_df %>%
    # keep only the numeric columns we need
    select(matches("^prior_|^epsilon_")) %>%
    pivot_longer(everything(),
                 names_to  = "metric",
                 values_to = "value") %>%
    mutate(Type = ifelse(grepl("^prior_",   metric), "Prior π", "Epsilon ε"))

  ggplot(metrics_long, aes(x = Type, y = value, fill = Type)) +
    geom_boxplot(width = .6, alpha = .6, outlier.shape = 16, outlier.size = 1.5) +
    scale_fill_manual(values = c("Prior π" = "#0073ff", "Epsilon ε" = "red"),
                      guide  = "none") +
    labs(title = paste0("Prior & Epsilon distribution (window ", win_tag, ")"),
         x = NULL, y = NULL) +
    theme_bw(base_size = 11)

  ggsave(paste0("graphs/Boxplot_Prior_Epsilon_", win_tag, ".png"),
         width = 5.5, height = 4)
}


# 2. Compute prior and epsilon for each subgroup ----

df_metrics <- df

# 2.a) Compute metrics on the threshold 330 kWh/m²
subgroup_metrics330 <- get_subgroup_metrics(sim_df  = simulation_results_CSV,
                                         real_df = df_filtered,
                                         lower = 290, upper = 375,
                                         threshold = 330,
                                         binwidth = 1)

print(summary(subgroup_metrics330))

plot_prior_eps_box(subgroup_metrics330, "330")

df_metrics <- df_metrics %>%
  left_join(subgroup_metrics330, by = c("type_logement", "periode_construction", "type_energie_chauffage"))

# 2.b) Compute metrics on the threshold 250 kWh/m²
subgroup_metrics250 <- get_subgroup_metrics(sim_df  = simulation_results_CSV,
                                         real_df = df_filtered,
                                         lower = 180, upper = 330,
                                         threshold = 250,
                                         binwidth = 1)

print(summary(subgroup_metrics250))

plot_prior_eps_box(subgroup_metrics250, "250")

df_metrics <- df_metrics %>%
  left_join(subgroup_metrics250, by = c("type_logement", "periode_construction", "type_energie_chauffage"))

# 2.c) Compute metrics on the threshold 420 kWh/m²
subgroup_metrics420 <- get_subgroup_metrics(sim_df  = simulation_results_CSV,
                                         real_df = df_filtered,
                                         lower = 330, upper = 800,
                                         threshold = 420,
                                         binwidth = 1)

print(summary(subgroup_metrics420))

plot_prior_eps_box(subgroup_metrics420, "420")

df_metrics <- df_metrics %>%
  left_join(subgroup_metrics420, by = c("type_logement", "periode_construction", "type_energie_chauffage"))


# 3. Enregistrer le résultat ----

write_csv(df_metrics, "alldpe_group_metrics_scott_new_2.csv")
