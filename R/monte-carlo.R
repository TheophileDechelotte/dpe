library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)

df <- read_csv("C:\\Users\\tdechelotte\\Desktop\\alldpe_v2.csv")
print(summary(df))

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
df_filtered <- df %>%
  filter(is_dpe_remplacant,
         !is_ancien_dpe,
         interval_ancien_dpe <= 90)

dpe_classes <- c(0, 70, 110, 180, 250, 330, 420, 800)
dpe_thresholds <- c(70, 110, 180, 250, 330, 420)
dpe_classes_colors <- c("darkgreen", "forestgreen","green", "gold", "orange", "darkorange", "red")


# 1. Fonctions utilitaires ----

# Fonction pour calculer la largeur optimale d'intervalle selon Freedman–Diaconis
FD_binwidth <- function(x) {
  x_clean <- x[!is.na(x)]
  if (length(x_clean) < 2) return(NA)
  rng <- diff(range(x_clean))
  if(rng == 0) return(NA)  # Return NA when range is zero; handle in compute_hist
  bw <- 2 * IQR(x_clean) / (length(x_clean)^(1/3))
  if(is.na(bw) || bw <= 0) { 
    bw <- rng / 100
    if(bw == 0) bw <- 1  # fallback
  }
  return(bw)
}

# Fonction pour calculer la largeur optimale d'intervalle selon la règle de Scott
Scott_binwidth <- function(x) {
  x_clean <- x[!is.na(x)]
  n <- length(x_clean)
  # Si moins de deux valeurs, on ne peut pas calculer
  if (n < 2) return(NA)
  # Écart-type
  sigma <- sd(x_clean)
  # Si écart-type nul ou NA, on ne peut pas appliquer la règle de Scott
  if (is.na(sigma) || sigma == 0) return(NA)
  # Règle de Scott : h = 3.5 * sigma / n^(1/3)
  bw <- 3.5 * sigma / (n^(1/3))
  # Fallback si le calcul donne NA ou une valeur non positive
  if (is.na(bw) || bw <= 0) {
    rng <- diff(range(x_clean))
    bw <- if (rng == 0) 1 else rng / 100
  }
  return(bw)
}

# Fonction pour calculer l'histogramme d'une variable en utilisant le binwidth optimal
compute_hist <- function(x) {
  x_clean <- x[!is.na(x)]
  if(length(x_clean) == 0) return(NULL)
  
  # Check if all values are identical.
  if(diff(range(x_clean)) == 0) {
    # Define a default breaks vector that spans a small interval around the constant value.
    constant_value <- min(x_clean)
    # Create breaks from constant_value - 0.5 to constant_value + 0.5 with a binwidth of 1
    breaks <- seq(constant_value - 0.5, constant_value + 0.5, by = 1)
  } else {
    bw <- Scott_binwidth(x_clean)
    # If the binwidth is NA or <= 0, set a fallback binwidth.
    if(is.na(bw) || bw <= 0) {
      bw <- diff(range(x_clean)) / 100
      if(bw == 0) bw <- 1
    }
    breaks <- seq(min(x_clean), max(x_clean), by = bw)
    # Ensure the last break is at least the maximum value.
    if(tail(breaks, 1) < max(x_clean)) {
      breaks <- c(breaks, max(x_clean))
    }
  }
  hist(x_clean, breaks = breaks, plot = FALSE)
}

# Fonction qui, à partir d'un histogramme, tire une valeur aléatoire :
# 1. Choix d'un bin avec une probabilité proportionnelle à la fréquence.
# 2. Tirage uniforme dans l'intervalle correspondant.
sample_from_hist <- function(h) {
  if(is.null(h) || sum(h$counts) == 0) return(NA)
  bin <- sample(seq_along(h$counts), size = 1, prob = h$counts)
  lower_bound <- h$breaks[bin]
  upper_bound <- h$breaks[bin + 1]
  runif(1, min = lower_bound, max = upper_bound)
}

# Fonction de simulation pour un sous-groupe de données
simulate_group <- function(data, n_sim = 1000000) {
  # Calcul des histogrammes pour chacune des variables de consommation
  histo_chauffage <- compute_hist(data$conso_chauffage)
  histo_ecs       <- compute_hist(data$conso_ecs)
  histo_froid     <- compute_hist(data$conso_froid)
  histo_eclairage <- compute_hist(data$conso_eclairage)
  histo_aux       <- compute_hist(data$conso_aux)
  
  # Simulation par échantillonnage pour chaque usage
  sim_chauffage <- replicate(n_sim, sample_from_hist(histo_chauffage))
  sim_ecs       <- replicate(n_sim, sample_from_hist(histo_ecs))
  sim_froid     <- replicate(n_sim, sample_from_hist(histo_froid))
  sim_eclairage <- replicate(n_sim, sample_from_hist(histo_eclairage))
  sim_aux       <- replicate(n_sim, sample_from_hist(histo_aux))
  
  # Calcul de la consommation totale
  sim_total <- sim_chauffage + sim_ecs + sim_froid + sim_eclairage + sim_aux
  
  # Renvoyer un data frame avec les résultats simulés
  data.frame(chauffage = sim_chauffage,
             ecs       = sim_ecs,
             froid     = sim_froid,
             eclairage = sim_eclairage,
             aux       = sim_aux,
             total     = sim_total)
}

# Fonction pour visualiser la simulation du sous-groupe souhaité
visualisation_group <- function(real_data, sim_data, x, y, z) {
  # Filtrer les données pour le sous-groupe
  selected_simulation <- sim_data %>%
    filter(type_logement == x,
           periode_construction == y,
           type_energie_chauffage == z)

  # Filtrer les données réelles pour le même sous-groupe
  filtered_data <- real_data %>%
    filter(type_logement == x,
           periode_construction == y,
           type_energie_chauffage == z)
  
  # Créer l'histogramme
  ggplot() +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.2) +
  geom_histogram(data = filtered_data,
                 aes(x = ep_conso_5_usages_m2, y = after_stat(density), fill = "Real EPCs"),
                 binwidth = 1,
                 position = "identity") +
  geom_freqpoly(data = selected_simulation, 
                aes(x = total, y= after_stat(density), color = "Virtual EPCs"),
                binwidth = 1, 
                linewidth = 0.5) +
  labs(title = paste0("Simulated EPC distribution (", x, ", ", y, ", ", z, ") (n = ", nrow(filtered_data), ")"),
       x = "Energy consumption (kWh/m²)",
       y = "Density") +
  scale_fill_manual(
      name   = NULL,
      values = c("Real EPCs" = "orange")
    ) +
    scale_color_manual(
      name   = NULL,
      values = c("Virtual EPCs" = "#0073ff")
    ) +
  xlim(0, 800) +
  theme_bw()

  ggsave(paste0("graphs/", x, "_", y, "_", z, "_simulation.png"), width = 8, height = 6)
}

# Fonction pour visualiser la simulation du sous-groupe souhaité
visualisation <- function(filtered_data, selected_simulation) {
  # Créer l'histogramme
  ggplot() +
  geom_vline(xintercept = dpe_thresholds, linetype = "dashed", color = "black", linewidth = 0.2) +
  geom_histogram(data = filtered_data,
                 aes(x = ep_conso_5_usages_m2, y = after_stat(density), fill = "Real EPCs"),
                 binwidth = 1,
                 position = "identity") +
  geom_freqpoly(data = selected_simulation, 
                aes(x = total, y= after_stat(density), color = "Virtual EPCs"),
                binwidth = 1, 
                linewidth = 0.5) +
  labs(title = paste0("Simulated EPC distribution (n = ", nrow(filtered_data), ")"),
       x = "Energy consumption (kWh/m²)",
       y = "Density") +
  scale_fill_manual(
      name   = NULL,
      values = c("Real EPCs" = "orange")
    ) +
    scale_color_manual(
      name   = NULL,
      values = c("Virtual EPCs" = "#0073ff")
    ) +
  xlim(0, 800) +
  theme_bw()

  ggsave(paste0("graphs/simulation.png"), width = 8, height = 6)
}

# 2. Simulation par sous-groupe ----

# On regroupe selon les trois variables de segmentation
group_simulation_results <- df_filtered %>%
  group_by(type_logement, periode_construction, type_energie_chauffage) %>%
  group_modify(~ {
    if(nrow(.x) < 100) {
      # Si le sous-groupe contient moins de 100 observations,
      # retourner un tibble vide (ou traiter autrement selon les besoins)
      return(tibble())
    } else {
      # Sinon, exécuter la simulation pour ce sous-groupe
      sim_df <- simulate_group(.x, n_sim = 1000000)
      sim_df$sim_id <- 1:nrow(sim_df)
      sim_df
    }
  })

# Enregistrer le résultat
write_csv(group_simulation_results, "C:\\Users\\tdechelotte\\Desktop\\group_simulation_scott.csv")

# Visualisation ----

# Exemple d'utilisation de la fonction de visualisation
visualisation_group(df_filtered, group_simulation_results, "maison individuelle", "1978-1982", "Gaz")


# 3. Simulation de l'ensemble des données ----

simulation_results <- simulate_group(df_filtered, n_sim = 1e6)

write_csv(simulation_results, "/Users/theophiledechelotte/Library/CloudStorage/OneDrive-Personnel/dpe-data/simulation_scott_post_shopping.csv")

visualisation(df_filtered, simulation_results)

