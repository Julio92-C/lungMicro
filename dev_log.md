# E-Breathomics Development Log

## Session 1 — 2026-04-06

### What was done

1. **Created CLAUDE.md** — Project guidance file for Claude Code with architecture, data flow, and tech stack details.

2. **Built Shiny app v0.1** with three core files:
   - `global.R` — Loads libraries (`shiny`, `bslib`, `tibble`, `dplyr`, `tidyr`, `stringr`, `readr`, `ggplot2`, `plotly`, `vegan`, `paletteer`, `pheatmap`, `patchwork`, `DT`), reads CSV datasets from `Datasets/` and `Metadata/` directories, runs shared data processing (pivot, merge, filtering).
   - `ui.R` — Dark-themed `bslib::page_navbar` layout with 6 tabs: Login, Overview, Alpha Diversity, Beta Diversity (PCoA), Taxonomy, Pathogenomics.
   - `server.R` — Reactive logic integrating analysis from existing R scripts: Kruskal-Wallis tests, Bray-Curtis PCoA with PERMANOVA, relative abundance stacked bars, pheatmap heatmaps.

3. **Copied logo** to `www/logo.png` for navbar branding.

4. **Bug fix** — Added `library(tibble)` to `global.R` to resolve `column_to_rownames` not found error.

5. **Pushed to GitHub** — Commit `338464f` on `main` branch at https://github.com/Julio92-C/lungMicro.git

### Data sources used
- `Datasets/bracken_arranged.csv` — Bracken taxonomic classification (wide format)
- `Datasets/sample_metadata.csv` — Sample-to-patient mapping (sample, ID, type)
- `Datasets/genetable_normdata.csv` — Gene-level data (GENE, TPM, DATABASE, RESISTANCE)
- `Datasets/abri_kraken2Bracken_merged.csv` — Merged Kraken2/Bracken with pathogenomics annotations
- `Metadata/patient_metadata.csv` — Patient demographics (ID, Age, Sex, Diagnosis, Treatment)

### App tabs summary
| Tab | Key analysis | Packages |
|-----|-------------|----------|
| Login | Research ID validation gate | shiny |
| Overview | Patient details + sample tables | DT |
| Alpha Diversity | Richness/Shannon boxplots, Kruskal-Wallis | vegan, plotly |
| Beta Diversity | PCoA (Bray-Curtis + Hellinger), PERMANOVA | vegan, plotly |
| Taxonomy | Relative abundance bars, genus counts, prevalence | plotly, paletteer |
| Pathogenomics | Gene heatmap (ARGs/VFs/MGEs), alpha diversity, gene table | pheatmap, vegan, DT |

### Known issues / next steps
- Logo image is a placeholder (Gemini-generated image)
- PCoA could be extended to 3D
- Report download (rmarkdown PDF) not yet implemented
- `shinyauthr` integration for proper authentication not yet added

---

## Session 2 — 2026-04-15

### What was done

1. **Dual-profile login system** — Added Patient/Clinician role-based access with `conditionalPanel` gating across all tabs.

2. **Diagnosis filter on Taxonomy tab** — Clinician-only `selectInput` for filtering by diagnosis (CVID default), reducing point density on relative abundance plot.

3. **Decoupled Overview filter from other tabs** — Fixed bug where the Overview Research ID filter (`clinician_filter_id`) cascaded into Alpha, Beta, Taxonomy, and Pathogenomics tabs. Introduced `overview_id()` reactive to keep the Overview filter local. Each tab now has independent filtering.

4. **Patient PDF report download** — Implemented downloadable PDF report for patients via `rmarkdown::render()` with TinyTeX:
   - Created `report_templates/patient_report.Rmd` — parameterised template with 4 sections:
     - **About You** — demographics table
     - **What We Found** — top 10 species bar chart (teal-to-purple gradient on dark navy)
     - **Antibiotic Resistance Profile** — ARG table + drug class bar chart (coral)
     - **Diversity** — Richness/Shannon per sample + interpretive text
   - Added `downloadButton` to Summary Report sidebar (patient-only)
   - Added `downloadHandler` in server.R with self-contained data prep
   - E-Breathomics branded colour theme on all figures
   - Added `library(knitr)` and `library(rmarkdown)` to global.R

5. **Commits pushed to GitHub:**
   - `2388abb` — Diagnosis filter + Overview filter decoupling
   - Patient PDF report (pending commit)

### Prerequisites added
- TinyTeX installation required: `tinytex::install_tinytex()` (one-time, ~250MB)

### Known issues / next steps
- Clinician PDF report not yet implemented (extended version with more detail)
- Logo image is a placeholder
- `shinyauthr` integration for proper authentication not yet added
