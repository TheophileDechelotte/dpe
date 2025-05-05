# Load packages
library(rdrobust)
library(rddensity)
library(tidyverse)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)

df <- read_csv("/Users/theophiledechelotte/Library/CloudStorage/OneDrive-UniversitéParisSciencesetLettres/dpe-data/alldpe_metrics_scott_v4.csv")

df$type_logement <- factor(df$type_logement)
df$periode_construction <- factor(df$periode_construction)
df$type_energie_chauffage <- factor(df$type_energie_chauffage)


