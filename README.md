# 🫁 lungMicro

**An interactive R Shiny dashboard for exploring the respiratory tract microbiome in immunodeficient patients.**

Built as part of the **E-Breathomics** dissertation project, lungMicro enables clinicians and researchers to investigate microbial communities from exhaled breath condensate (EBC) and sputum samples — bridging metagenomics with bedside decision-making.

---

## 🩺 Clinical Context

Primary immunodeficiencies (e.g. CVID, XLA) predispose patients to recurrent respiratory infections. Understanding the lung microbiome composition and its associated resistome/virulome can guide targeted antimicrobial therapy and surveillance. This app provides a patient-level, interactive view of metagenomic profiling results generated from Kraken2/Bracken classification pipelines.

---

## ✨ Features

### 🔐 Secure Login
Patients and researchers authenticate using a unique Research ID, gating access to individual-level clinical and microbiome data.

### 📋 Patient Overview
At-a-glance summary cards displaying **Patient ID**, **Diagnosis**, and **Treatment**, alongside detailed metadata and sample availability tables.

### 📊 Alpha Diversity
Interactive boxplots and per-sample dot plots for **Richness** and **Shannon** indices, with Kruskal-Wallis statistical testing. Group comparisons by sample type (Exhale / Sputum) or immunodeficiency diagnosis.

### 🔗 Beta Diversity (PCoA)
Bray-Curtis dissimilarity-based Principal Coordinates Analysis with confidence ellipses and **PERMANOVA** results — visualising community-level differences across patient groups.

### 🦠 Taxonomic Profiles
Stacked bar charts of relative genus abundance, filterable by sample type and top-N genera. Complemented by total count and prevalence plots to identify dominant and widespread taxa.

### 🧬 Pathogenomics Heatmap
Gene presence/abundance heatmap (log TPM) for antimicrobial resistance genes (**CARD**), virulence factors (**VFDB**), and mobile genetic elements (**PlasmidFinder**). Includes per-sample Shannon diversity of genetic elements and a searchable gene summary table.

---

## 🛠️ Tech Stack

| Layer | Tools |
|-------|-------|
| **Framework** | R Shiny + `bslib` (Bootstrap 5, dark theme) |
| **Visualisation** | `plotly`, `ggplot2`, `pheatmap` |
| **Ecology** | `vegan` (diversity indices, PCoA, PERMANOVA) |
| **Data wrangling** | `dplyr`, `tidyr`, `stringr`, `readr` |
| **Tables** | `DT` |
| **Typography** | Google Fonts — Inter |

---

## 🚀 Getting Started

```bash
# Clone the repository
git clone https://github.com/Julio92-C/lungMicro.git
cd lungMicro

# Launch the app
Rscript -e "shiny::runApp('.')"
```

Or open the project in **RStudio** and click **Run App**.

---

## 📁 Project Structure

```
lungMicro/
├── global.R    # Libraries, data loading, shared palettes & theme
├── ui.R        # Dashboard layout (bslib page_navbar)
├── server.R    # Reactive logic, plots, and statistical tests
└── www/        # Static assets (logo, images)
```

---

## 📄 License

Part of the E-Breathomics dissertation project.
