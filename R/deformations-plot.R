# Load packages
library(rdrobust)
library(rddensity)
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)

df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_metrics_scott_v4.csv")

df$type_logement <- factor(df$type_logement)
df$periode_construction <- factor(df$periode_construction)
df$type_energie_chauffage <- factor(df$type_energie_chauffage)

df_summary <- df %>% group_by(type_logement, periode_construction, type_energie_chauffage) %>%
summarise(
        prior_330 = mean(prior_330, na.rm = TRUE),
        cert_def_330 = mean(cert_def_330, na.rm = TRUE),
        shop_def_330 = mean(shop_def_330, na.rm = TRUE),
        prior_250 = mean(prior_250, na.rm = TRUE),
        cert_def_250 = mean(cert_def_250, na.rm = TRUE),
        shop_def_250 = mean(shop_def_250, na.rm = TRUE),
        prior_420 = mean(prior_420, na.rm = TRUE),
        cert_def_420 = mean(cert_def_420, na.rm = TRUE),
        shop_def_420 = mean(shop_def_420, na.rm = TRUE), 
        .groups = "drop") %>%
  select(type_logement, periode_construction, type_energie_chauffage, prior_330, cert_def_330, shop_def_330, prior_250, cert_def_250, shop_def_250, prior_420, cert_def_420, shop_def_420) %>%
  filter(!is.na(prior_330) &
         !is.na(cert_def_330) &
         !is.na(shop_def_330) &
         !is.na(prior_250) &
         !is.na(cert_def_250) &
         !is.na(shop_def_250) &
         !is.na(prior_420) &
         !is.na(cert_def_420) &
         !is.na(shop_def_420)) %>%
  ungroup()

# Fonction helper
plot_deformations_stacked <- function(type_logement_sel,
                                     periode_construction_sel,
                                     type_energie_chauffage_sel) {
  # 1) filtre
  df_sel <- df %>%
    filter(type_logement       == type_logement_sel,
           periode_construction == periode_construction_sel,
           type_energie_chauffage == type_energie_chauffage_sel)
  
  # 2) passage en “long” et extraction des colonnes metric & cutoff
  df_long <- df_sel %>%
    pivot_longer(
      cols = matches("^(prior|cert_def|shop_def)_"),
      names_to    = c("metric", "cutoff"),
      names_pattern = "(prior|cert_def|shop_def)_(\\d+)",
      values_to   = "value"
    ) %>%
    mutate(
      # pour ordonner le stacking bottom→top
      metric = factor(metric,
                      levels = c("shop_def", "cert_def","prior"),
                      labels = c("Shop_def", "Cert_def", "Prior")),
      # remettre cutoff dans l’ordre 250,330,420
      cutoff = factor(cutoff,
                      levels = c("250","330","420"),
                      labels = c("Seuil 250 kWh/m².an",
                                 "Seuil 330 kWh/m².an",
                                 "Seuil 420 kWh/m².an"))
    )
  
  df_counts <- df_long %>%
    group_by(cutoff, metric) %>%
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop")
  
  # 4) tracer le stacked‐bar
  ggplot(df_counts, aes(x = cutoff, y = value, fill = metric)) +
    geom_col() +
    labs(
      x = NULL,
      y = "Effectif",
      fill = NULL
    ) +
    theme_bw() +
    theme(
      axis.text.x     = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position = "right"
    )
}

plot_deformations_stacked(
  type_logement = "appartement",
  periode_construction = "avant 1948",
  type_energie_chauffage = "Electricite")
