# Charger les bibliothèques nécessaires
library(tidyverse)
library(scales)


# Lire les données
df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_v2.csv")

simulation_results_CSV <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_simulation_scott.csv")

# Regroupement des types d'énergie
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

# Filtrage des DPE actifs et transformation en facteurs pour la segmentation
df_shopping <- df %>%
  filter(is_dpe_remplacant,
         !is_ancien_dpe,
         interval_ancien_dpe <= 90)

df_certif <- df %>%
  filter(!is_dpe_remplacant)

dpe_classes <- c(0, 70, 110, 180, 250, 330, 420, 800)
dpe_thresholds <- c(70, 110, 180, 250, 330, 420)
dpe_classes_colors <- c("darkgreen", "forestgreen","green", "gold", "orange", "darkorange", "red")


# --- Fonctions utilitaires ----

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
                            real_data_bfore_shopping,
                            sim_data,
                            lower = 250,
                            upper = 420,
                            threshold = 330,
                            binwidth  = 1) {
  sim_window  <- sim_data  %>% filter(total >= lower,
                                       total <  upper)
  real_window <- real_data %>% filter(ep_conso_5_usages_m2 >= lower,
                                       ep_conso_5_usages_m2 <  upper)
  real_window2 <- real_data_bfore_shopping %>% filter(ep_conso_5_usages_m2 >= lower,
                                       ep_conso_5_usages_m2 <  upper)                                     

  if (nrow(sim_window) == 0 || nrow(real_window) == 0 || nrow(real_window2) == 0) {
    return(list(certification_deformation = NA_real_,
                shopping_deformation      = NA_real_,
                epsilon_shopping          = NA_real_))
  }
    
  breaks_seq <- seq(lower, upper, by = binwidth)

  h_sim  <- hist(sim_window$total,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)           # density integrates to 1
  h_real <- hist(real_window$ep_conso_5_usages_m2,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)
  h_real2 <- hist(real_window2$ep_conso_5_usages_m2,
                 breaks = breaks_seq,
                 plot   = FALSE,
                 prob   = TRUE)

  dens_sim  <- h_sim$density
  dens_real <- h_real$density
  dens_real2 <- h_real2$density
  mids      <- h_real$mids              # same for both histograms
  dens_star <- ifelse(mids < threshold,
                      pmax(dens_real, dens_sim),
                      pmin(dens_real, dens_sim))
  dens_star2 <- ifelse(mids < threshold,
                      pmax(dens_real2, dens_sim),
                      pmin(dens_real2, dens_sim))

  # normalise (f* may not integrate to 1 after max/min step)
  total_mass   <- sum(dens_star) * binwidth
  if (total_mass == 0) return(NA_real_)
  dens_star    <- dens_star / total_mass
  prior_star <- sum(dens_star[mids < threshold]) * binwidth

  total_mass2   <- sum(dens_star2) * binwidth
  if (total_mass2 == 0) return(NA_real_)
  dens_star2    <- dens_star2 / total_mass2
  prior_star2  <- sum(dens_star2[mids < threshold]) * binwidth

  prior_sim  <- compute_prior(sim_window,
                              lower     = lower,
                              upper     = upper,
                              threshold = threshold)

  certification_deformation <- (prior_star2 - prior_sim)
  shopping_deformation      <- (prior_star  - prior_star2)
  epsilon_shopping          <- shopping_deformation / (1 - prior_star2)

  list(certification_deformation = certification_deformation,
       shopping_deformation      = shopping_deformation,
       epsilon_shopping          = epsilon_shopping)
}

# Helper function: compute π and ε for every subgroup in one shot
get_subgroup_metrics <- function(sim_df,
                                 real_df,
                                 real_data_bfore_shopping,
                                 lower        = 250,
                                 upper        = 420,
                                 threshold    = 330,
                                 binwidth     = 1) {

  prior_col  <- paste0("prior_",  threshold)
  cert_col   <- paste0("cert_def_",  threshold)
  shop_col   <- paste0("shop_def_",  threshold)
  eps_col    <- paste0("eps_shop_",  threshold)

  subgroups_tbl <- sim_df %>%
    distinct(type_logement, periode_construction, type_energie_chauffage)

  subgroups_tbl %>%
    mutate(
      metrics = pmap(
        list(type_logement, periode_construction, type_energie_chauffage),
        function(tl, pc, tec) {

          sim_sub  <- sim_df  %>% filter(type_logement == tl,
                                         periode_construction == pc,
                                         type_energie_chauffage == tec)

          real_sub <- real_df %>% filter(type_logement == tl,
                                         periode_construction == pc,
                                         type_energie_chauffage == tec)

          real_sub2 <- real_data_bfore_shopping %>% filter(type_logement == tl,
                                                           periode_construction == pc,
                                                           type_energie_chauffage == tec)

          if (nrow(sim_sub) == 0 || nrow(real_sub) == 0 || nrow(real_sub2) == 0) {
            tibble(!!prior_col := NA_real_,
                   !!cert_col  := NA_real_,
                   !!shop_col  := NA_real_,
                   !!eps_col   := NA_real_)
          } else {
            pi_val  <- compute_prior(sim_sub,
                                     lower = lower, upper = upper,
                                     threshold = threshold)

            eps_lst <- compute_epsilon(real_sub, real_sub2, sim_sub,
                                       lower = lower, upper = upper,
                                       threshold = threshold, binwidth = binwidth)

            tibble(!!prior_col := pi_val,
                   !!cert_col  := eps_lst$certification_deformation,
                   !!shop_col  := eps_lst$shopping_deformation,
                   !!eps_col   := eps_lst$epsilon_shopping)
          }
        }
      )
    ) %>%
    unnest(cols = metrics)
}

# Computes prior & epsilon metrics on the *full* data (no grouping)
get_metrics <- function(sim_df,
                        real_df,
                        real_data_bfore_shopping,
                        lower     = 250,
                        upper     = 420,
                        threshold = 330,
                        binwidth  = 1) {
  
  # 1. Prior π: share of simulations in [lower, upper) below threshold
  prior_val <- compute_prior(sim_df,
                             lower     = lower,
                             upper     = upper,
                             threshold = threshold)

  # 2. Epsilon & decompositions
  eps_lst <- compute_epsilon(real_df,
                             real_data_bfore_shopping,
                             sim_df,
                             lower     = lower,
                             upper     = upper,
                             threshold = threshold,
                             binwidth  = binwidth)

  # 3. Assemble into a one‐row tibble
  tibble(
    !!paste0("prior_", threshold)  := prior_val,
    !!paste0("cert_def_", threshold) := eps_lst$certification_deformation,
    !!paste0("shop_def_", threshold) := eps_lst$shopping_deformation,
    !!paste0("eps_shop_", threshold) := eps_lst$epsilon_shopping
  )
}

# Helper – one box‑plot for prior & epsilon in a metrics tibble 
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


# Compute prior and epsilon for each subgroup ----

# Initialize the df_subgroup_metrics dataframe
df_subgroup_metrics <- df

subgroup_metrics330 <- get_subgroup_metrics(sim_df  = simulation_results_CSV,
                                            real_df = df_shopping,
                                            real_data_bfore_shopping = df_certif,
                                            lower = 250, upper = 420,
                                            threshold = 330)

print(summary(subgroup_metrics330))

df_subgroup_metrics <- df_subgroup_metrics %>%
  left_join(subgroup_metrics330, by = c("type_logement", "periode_construction", "type_energie_chauffage"))

subgroup_metrics250 <- get_subgroup_metrics(sim_df  = simulation_results_CSV,
                                            real_df = df_shopping,
                                            real_data_bfore_shopping = df_certif,
                                            lower = 180, upper = 330,
                                            threshold = 250)

print(summary(subgroup_metrics250))

df_subgroup_metrics <- df_subgroup_metrics %>%
  left_join(subgroup_metrics250, by = c("type_logement", "periode_construction", "type_energie_chauffage"))

subgroup_metrics420 <- get_subgroup_metrics(sim_df  = simulation_results_CSV,
                                            real_df = df_shopping,
                                            real_data_bfore_shopping = df_certif,
                                            lower = 330, upper = 800,
                                            threshold = 420)

print(summary(subgroup_metrics420))

df_subgroup_metrics <- df_subgroup_metrics %>%
  left_join(subgroup_metrics420, by = c("type_logement", "periode_construction", "type_energie_chauffage"))

# Enregistrer le résultat
write_csv(df_subgroup_metrics, "data/alldpe_metrics_scott_v4.csv")


# Compute prior and epsilon for the full dataset ----

# For the 250–420 window with threshold 330
metrics330 <- get_metrics(
  sim_df                   = simulation_results_CSV,
  real_df                  = df_shopping,
  real_data_bfore_shopping = df_certif,
  lower     = 250,
  upper     = 420,
  threshold = 330,
  binwidth  = 1
)

print(summary(metrics330))

metrics250 <- get_metrics(
  sim_df                   = simulation_results_CSV,
  real_df                  = df_shopping,
  real_data_bfore_shopping = df_certif,
  lower     = 180,
  upper     = 330,
  threshold = 250,
  binwidth  = 1
)
print(summary(metrics250))

metrics420 <- get_metrics(
  sim_df                   = simulation_results_CSV,
  real_df                  = df_shopping,
  real_data_bfore_shopping = df_certif,
  lower     = 330,
  upper     = 800,
  threshold = 420,
  binwidth  = 1
)
print(summary(metrics420))

df_metrics <- cbind(
  metrics250,
  metrics330,
  metrics420
)

# Enregistrer le résultat
write_csv(df_metrics, "/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_metrics_scott_v5.csv")


