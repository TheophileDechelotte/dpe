# Load packages
library(rdrobust)
library(rddensity)
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)
library(gridExtra)
library(ggpattern)

df_subgroup <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_metrics_scott_v4.csv")

df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_metrics_scott_v5.csv")

df_subgroup$type_logement <- factor(df_subgroup$type_logement)
df_subgroup$periode_construction <- factor(df_subgroup$periode_construction)
df_subgroup$type_energie_chauffage <- factor(df_subgroup$type_energie_chauffage)

df_subgroup_summary <- df_subgroup %>% 
  group_by(type_logement, periode_construction, type_energie_chauffage) %>%
  summarise(prior_330 = mean(prior_330, na.rm = TRUE),
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

df_summary <- df %>%
  filter(!is.na(prior_330) &
         !is.na(cert_def_330) &
         !is.na(shop_def_330) &
         !is.na(prior_250) &
         !is.na(cert_def_250) &
         !is.na(shop_def_250) &
         !is.na(prior_420) &
         !is.na(cert_def_420) &
         !is.na(shop_def_420))


plot_subgroup_deformation <- function(type_logement_sel,
                                             periode_construction_sel,
                                             type_energie_chauffage_sel) {
  
  # 1) grab your one‐row summary
  df_sel <- df_subgroup_summary %>%
    filter(type_logement        == type_logement_sel,
           periode_construction == periode_construction_sel,
           type_energie_chauffage == type_energie_chauffage_sel)
  
  # 2) build a single long df with both “before” and “after”
  df_plot <- bind_rows(
    # → “before” (the left‐hand bars: prior / cert_def / shop_def)
    df_sel %>%
      pivot_longer(
        cols = matches("^(prior|cert_def|shop_def)_"),
        names_to      = c("metric","cutoff"),
        names_pattern = "(prior|cert_def|shop_def)_(\\d+)",
        values_to     = "value"
      ) %>%
      mutate(
        cutoff   = factor(cutoff,
                          levels = c("250","330","420")),
        category = factor(metric,
                          levels = c("shop_def", "cert_def", "prior"),
                          labels = c("Shopping (+)",
                                     "Certification (+)",
                                     "Baseline (efficient)")),
        side     = "before"
      ) %>%
      select(cutoff, category, value, side),
    
    # → “after” (the right‐hand bars: remaining / removed by cert / removed by shop)
    tibble(
      cutoff = factor(c("250","330","420"),
                      levels = c("250","330","420")),
      Remaining     = c(
        1 - df_sel$prior_250 - df_sel$cert_def_250 - df_sel$shop_def_250,
        1 - df_sel$prior_330 - df_sel$cert_def_330 - df_sel$shop_def_330,
        1 - df_sel$prior_420 - df_sel$cert_def_420 - df_sel$shop_def_420
      ),
      `Removed: Cert` = c(df_sel$cert_def_250,
                         df_sel$cert_def_330,
                         df_sel$cert_def_420),
      `Removed: Shop` = c(df_sel$shop_def_250,
                         df_sel$shop_def_330,
                         df_sel$shop_def_420)
    ) %>%
      pivot_longer(-cutoff,
                   names_to  = "segment",
                   values_to = "value") %>%
      mutate(
        category = factor(segment,
                          levels = c("Removed: Shop",
                                     "Removed: Cert",
                                     "Remaining"),
                          labels = c("Shopping (-)",
                                     "Certification (-)",
                                     "Remaining (inefficient)")),
        side     = "after"
      ) %>%
      select(cutoff, category, value, side)
  )
  
  # 3) compute numeric x positions so we can dodge left/right
  df_plot <- df_plot %>%
    mutate(
      x_pos = as.numeric(cutoff) + if_else(side == "before", -0.2, +0.2)
    )
  
  # 4) single ggplot call
  ggplot(df_plot,
         aes(x = x_pos, y = value,
             fill  = category,
             color = category)) +
    geom_col(width = 0.35) +
    scale_x_continuous(
      breaks = 1:3,
      labels = c("D/E", "E/F", "F/G")
    ) +
    scale_color_manual(
      values = c(
        "Certification (-)" = "orange",
        "Shopping (-)"      = "red",
        "Baseline (efficient)"          = "grey50",
        "Certification (+)" = "orange",
        "Shopping (+)"      = "red",
        "Remaining (inefficient)"         = "grey"
      )
    ) +
    scale_fill_manual(
      breaks = c("Baseline (efficient)",
                 "Certification (+)",
                 "Shopping (+)",
                 "Remaining (inefficient)",
                 "Certification (-)",
                 "Shopping (-)"),
      values = c(
        "Baseline (efficient)"          = "grey50",
        "Certification (+)" = "orange",
        "Shopping (+)"      = "red",
        "Remaining (inefficient)"         = "grey",
        "Certification (-)" = "white",
        "Shopping (-)"      = "white"
      )
    ) +
    labs(
      title = paste0("EPCs deformations [", type_logement_sel, ", ",periode_construction_sel, ", ", type_energie_chauffage_sel, "]"),
      x    = "Cutoff",
      y    = "Share",
      fill = NULL
    ) +
    theme_bw() +
    guides(color = "none") +
    ylim(0, 1) +
    theme(
      axis.text.x       = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position    = "right"
    )
  ggsave(paste0("graphs/shopping-outcome/", type_logement_sel, "_", periode_construction_sel, "_", type_energie_chauffage_sel, "_deformations.png"), width = 8, height = 6)
  }


plot_subgroup_deformation(
  type_logement_sel        = "appartement",
  periode_construction_sel = "avant 1948",
  type_energie_chauffage_sel = "Gaz"
)

plot_deformation <- function(df_sel) {

  df_plot <- bind_rows(
    # → “before” (the left‐hand bars: prior / cert_def / shop_def)
    df_sel %>%
      pivot_longer(
        cols = matches("^(prior|cert_def|shop_def)_"),
        names_to      = c("metric","cutoff"),
        names_pattern = "(prior|cert_def|shop_def)_(\\d+)",
        values_to     = "value"
      ) %>%
      mutate(
        cutoff   = factor(cutoff,
                          levels = c("250","330","420")),
        category = factor(metric,
                          levels = c("shop_def", "cert_def", "prior"),
                          labels = c("Shopping (+)",
                                     "Certification (+)",
                                     "Baseline (efficient)")),
        side     = "before"
      ) %>%
      select(cutoff, category, value, side),
    
    # → “after” (the right‐hand bars: remaining / removed by cert / removed by shop)
    tibble(
      cutoff = factor(c("250","330","420"),
                      levels = c("250","330","420")),
      Remaining     = c(
        1 - df_sel$prior_250 - df_sel$cert_def_250 - df_sel$shop_def_250,
        1 - df_sel$prior_330 - df_sel$cert_def_330 - df_sel$shop_def_330,
        1 - df_sel$prior_420 - df_sel$cert_def_420 - df_sel$shop_def_420
      ),
      `Removed: Cert` = c(df_sel$cert_def_250,
                         df_sel$cert_def_330,
                         df_sel$cert_def_420),
      `Removed: Shop` = c(df_sel$shop_def_250,
                         df_sel$shop_def_330,
                         df_sel$shop_def_420)
    ) %>%
      pivot_longer(-cutoff,
                   names_to  = "segment",
                   values_to = "value") %>%
      mutate(
        category = factor(segment,
                          levels = c("Removed: Shop",
                                     "Removed: Cert",
                                     "Remaining"),
                          labels = c("Shopping (-)",
                                     "Certification (-)",
                                     "Remaining (inefficient)")),
        side     = "after"
      ) %>%
      select(cutoff, category, value, side)
  )
  
  # 3) compute numeric x positions so we can dodge left/right
  df_plot <- df_plot %>%
    mutate(
      x_pos = as.numeric(cutoff) + if_else(side == "before", -0.2, +0.2)
    )
  
  # 4) single ggplot call
  ggplot(df_plot,
         aes(x = x_pos, y = value,
             fill  = category,
             color = category)) +
    geom_col(width = 0.35) +
    scale_x_continuous(
      breaks = 1:3,
      labels = c("D/E", "E/F", "F/G")
    ) +
    scale_color_manual(
      values = c(
        "Certification (-)" = "orange",
        "Shopping (-)"      = "red",
        "Baseline (efficient)"          = "grey50",
        "Certification (+)" = "orange",
        "Shopping (+)"      = "red",
        "Remaining (inefficient)"         = "grey"
      )
    ) +
    scale_fill_manual(
      breaks = c("Baseline (efficient)",
                 "Certification (+)",
                 "Shopping (+)",
                 "Remaining (inefficient)",
                 "Certification (-)",
                 "Shopping (-)"),
      values = c(
        "Baseline (efficient)"          = "grey50",
        "Certification (+)" = "orange",
        "Shopping (+)"      = "red",
        "Remaining (inefficient)"         = "grey",
        "Certification (-)" = "white",
        "Shopping (-)"      = "white"
      )
    ) +
    labs(
      title = paste0("EPCs deformations"),
      x    = "Cutoff",
      y    = "Share",
      fill = NULL
    ) +
    theme_bw() +
    guides(color = "none") +
    ylim(0, 1) +
    theme(
      axis.text.x       = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position    = "right"
    )
  ggsave(paste0("graphs/shopping-outcome/deformations.png"), width = 8, height = 6)
  }

plot_deformation(
  df_sel = df_summary
)
