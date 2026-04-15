# global.R — Load libraries, data, and shared processing functions

# ── Libraries ────────────────────────────────────────────────────────────────
library(shiny)
library(bslib)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(plotly)
library(vegan)
library(paletteer)
library(pheatmap)
library(patchwork)
library(DT)
library(knitr)
library(rmarkdown)

# ── Data paths ───────────────────────────────────────────────────────────────
DATA_DIR     <- "C:/Users/julio/OneDrive/Desktop/Lung_microbiome/Datasets"
METADATA_DIR <- "C:/Users/julio/OneDrive/Desktop/Lung_microbiome/Metadata"

# ── Load raw data ────────────────────────────────────────────────────────────
bracken_arranged         <- read_csv(file.path(DATA_DIR, "bracken_arranged.csv"),
                                     show_col_types = FALSE)
sample_metadata          <- read_csv(file.path(DATA_DIR, "sample_metadata.csv"),
                                     show_col_types = FALSE)
genetable_normdata_raw   <- read_csv(file.path(DATA_DIR, "genetable_normdata.csv"),
                                     show_col_types = FALSE)
abri_kraken2Bracken_raw  <- read_csv(file.path(DATA_DIR, "abri_kraken2Bracken_merged.csv"),
                                     show_col_types = FALSE)
patient_metadata         <- read_csv(file.path(METADATA_DIR, "patient_metadata.csv"),
                                     show_col_types = FALSE)

# ── Clean metadata ───────────────────────────────────────────────────────────
patient_metadata <- patient_metadata %>% distinct(ID, .keep_all = TRUE)

# ── Colour palettes (consistent across tabs) ─────────────────────────────────
ip_palette <- c(CVID = "#4a91e2", XLA = "#009e74", HG = "#8c57b4", SID = "#d55e00")
sample_palette <- c(Exhale = "#93e9f5", Sputum = "#fcf260")

# ── Bracken pivot (shared by alpha, beta & taxonomy tabs) ────────────────────
bracken_pivoted <- bracken_arranged %>%
  pivot_longer(
    cols = c(4:27, 30:56),
    names_to  = "sample",
    values_to = "count"
  ) %>%
  filter(
    !grepl("root|Homo sapiens|cellular organisms|unclassified|Bacteria|environmental samples", name),
    !name %in% c("Clostridiaceae",
                  "Gammaproteobacteria incertae sedis",
                  "Betaproteobacteria incertae sedis",
                  "Clostridia incertae sedis"),
    !is.na(name)
  ) %>%
  group_by(sample) %>%
  filter(count > 5) %>%
  mutate(
    log_count  = log(count + 1),
    Percentage = log_count / sum(log_count)
  ) %>%
  ungroup()

# Merge with metadata
bracken_merged <- bracken_pivoted %>%
  inner_join(sample_metadata, by = "sample") %>%
  inner_join(patient_metadata, by = "ID")

# ── Gene table (pathogenomics) merged with metadata ──────────────────────────
genetable_normdata <- genetable_normdata_raw %>%
  inner_join(patient_metadata, by = "ID")

# ── ABRicate + Kraken2/Bracken merged (species ↔ gene ↔ resistance) ──────────
abri_kraken2Bracken <- abri_kraken2Bracken_raw %>%
  distinct(GENE, sequence, .keep_all = TRUE) %>%
  filter(
    !grepl("Terrabacteria group|Bacteroidota/Chlorobiota group|FCB group|Cyanobacteriota/Melainabacteria group", name),
    str_count(name, "\\S+") > 1
  )

# ── Valid patient IDs for login ───────────────────────────────────────────────
valid_patient_ids <- sort(unique(patient_metadata$ID))

# ── Clinician access code ────────────────────────────────────────────────────
CLINICIAN_CODE <- "ebreath2024"

# ── E-Breathomics bslib theme ────────────────────────────────────────────────
e_theme <- bs_theme(
  bg      = "#0b172a",
  fg      = "#FAF0E6",
  primary = "#2EC4B6",
  secondary = "#8E44AD",
  base_font = font_google("Inter"),
  "navbar-bg" = "#0b172a"
)
