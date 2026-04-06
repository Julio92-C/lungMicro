# 🫁 lungMicro

**An interactive R Shiny dashboard for exploring the respiratory tract microbiome in immunodeficient patients.**

Built as part of the **E-Breathomics** dissertation project, lungMicro enables clinicians and researchers to investigate microbial communities from exhaled breath condensate (EBC) and sputum samples — bridging metagenomics with bedside decision-making.

---

## 🩺 Clinical Context

Primary immunodeficiencies (e.g. CVID, XLA) predispose patients to recurrent respiratory infections. Understanding the lung microbiome composition and its associated resistome/virulome can guide targeted antimicrobial therapy and surveillance. This app provides a patient-level, interactive view of metagenomic profiling results generated from Kraken2/Bracken classification pipelines.

---

## ✨ Features

### 🔐 Dual-Profile Login
Role-based access via a **Patient** or **Clinician** toggle:
- **Patients** select their Research ID and see only their own data across all tabs.
- **Clinicians** enter a shared access code and can browse the full cohort or filter by any Research ID.

### 📋 Patient Overview
Summary value cards for **Patient ID** (with sample count), **Diagnosis** (with age & sex), and **Treatment**. The patient view includes:
- 🦠 **Top 5 taxa carrying genetic elements** — ranked by distinct GEs, with colour-coded database badges (CARD, VFDB, PlasmidFinder).
- 🧫 **GE pie chart** — breakdown of detected genetic elements by database.

Clinicians see the full patient details and sample availability tables instead.

### 📊 Alpha Diversity
Interactive boxplots and per-sample dot plots for **Richness** and **Shannon** indices, with Kruskal-Wallis statistical testing. Patients see sample-type comparisons only; clinicians can also group by immunodeficiency diagnosis.

### 🔗 Beta Diversity (PCoA)
Bray-Curtis dissimilarity-based Principal Coordinates Analysis with confidence ellipses and **PERMANOVA** results. In patient view, the full cohort PCoA is displayed with the patient's own samples highlighted (larger, opaque markers).

### 🦠 Taxonomic Profiles
Stacked bar charts of relative genus abundance, filterable by sample type and top-N genera (default 20 for patients, 30 for clinicians). Complemented by total count and prevalence plots. Includes a full-screen **expand button** for the bar chart.

### 🧬 Pathogenomics Heatmap
Gene presence/abundance heatmap (log TPM) for antimicrobial resistance genes (**CARD**), virulence factors (**VFDB**), and mobile genetic elements (**PlasmidFinder**). Patients see a per-sample bar chart of GE diversity; clinicians see the diagnosis-level boxplot. Includes a full-screen **expand button** for the heatmap and a searchable gene summary table.

### 📝 Summary Report
Dedicated tab (both profiles) with a configurable **Top N species** slider (default 5). Displays:
- Species & drug resistance profile table (species, Bracken count, ARG count, resistance classes)
- Species abundance bar chart (log10)
- Drug resistance classes bar chart (number of ARGs per class)

Clinicians can filter by Research ID; patients are automatically scoped to their own data.

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
