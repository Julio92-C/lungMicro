# ui.R — E-Breathomics Dashboard layout

page_navbar(
  title = tags$span(
    tags$img(
      src = "logo.png", height = "30px",
      style = "margin-right:10px;", onerror = "this.style.display='none'"
    ),
    "E-Breathomics"
  ),
  theme  = e_theme,
  id     = "main_nav",
  header = tags$head(
    tags$style(HTML("
      /* Login card centring */
      .login-panel { max-width:420px; margin:10vh auto; }
      .card        { background:#112240; border:1px solid #2EC4B6; }
      .card-header { background:#2EC4B6; color:#0b172a; font-weight:600; }
      /* plotly background */
      .js-plotly-plot .plotly .main-svg { background:transparent !important; }
      /* Compact value boxes */
      .bslib-value-box .value-box-value { font-size: 1.2rem !important; }
      .bslib-value-box .value-box-title { font-size: 0.85rem !important; }
      .bslib-value-box .value-box-showcase .fa { font-size: 2rem !important; }
    "))
  ),

  # ── Login tab (shown first) ────────────────────────────────────────────────
  nav_panel(
    title = "Login",
    icon  = icon("lock"),
    value = "login_tab",
    div(
      class = "login-panel",
      div(
        class = "card",
        div(class = "card-header", "Secure Gateway"),
        div(
          class = "card-body",
          radioButtons("login_role", "I am a:",
                       choices  = c("Patient", "Clinician"),
                       selected = "Patient", inline = TRUE),
          conditionalPanel(
            condition = "input.login_role == 'Patient'",
            selectInput("user_id_field", "Research ID",
                        choices = c("Select your ID" = "", valid_patient_ids))
          ),
          conditionalPanel(
            condition = "input.login_role == 'Clinician'",
            passwordInput("clinician_password", "Clinician Code")
          ),
          actionButton("login_button", "Enter Dashboard",
                       class = "btn-primary w-100 mt-2")
        )
      )
    )
  ),

  # ── Overview tab ───────────────────────────────────────────────────────────
  nav_panel(
    title = "Overview",
    icon  = icon("chart-pie"),
    value = "overview_tab",
    conditionalPanel(
      condition = "output.is_clinician == true",
      div(
        class = "mb-3",
        selectInput("clinician_filter_id", "Filter by Research ID",
                    choices = c("All" = "All", valid_patient_ids),
                    width = "300px")
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Patient ID",    uiOutput("ov_id_card"), showcase = icon("id-card"),
                theme = "primary"),
      value_box("Diagnosis",     uiOutput("ov_diag_card"), showcase = icon("stethoscope"),
                theme = "secondary"),
      value_box("Treatment",     textOutput("ov_treat"), showcase = icon("pills"),
                theme = "info")
    ),
    # Patient view: Top taxa (left) + GE pie chart (right)
    conditionalPanel(
      condition = "output.is_clinician != true",
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header(icon("bacteria"), " Top Taxa Carrying Genetic Elements"),
          uiOutput("ov_top_taxa_list")
        ),
        card(
          card_header("Genetic Elements Detected"),
          plotlyOutput("ov_ge_pie", height = "320px")
        )
      )
    ),
    # Clinician view: original tables
    conditionalPanel(
      condition = "output.is_clinician == true",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Patient Details"),
          DTOutput("ov_table")
        ),
        card(
          card_header("Samples Available"),
          DTOutput("ov_samples_clin")
        )
      )
    )
  ),

  # ── Alpha Diversity tab ────────────────────────────────────────────────────
  nav_panel(
    title = "Alpha Diversity",
    icon  = icon("chart-bar"),
    value = "alpha_tab",
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        selectInput("alpha_index", "Diversity Index",
                    choices = c("Richness", "Shannon")),
        conditionalPanel(
          condition = "output.is_clinician == true",
          selectInput("alpha_group", "Group by",
                      choices = c("Sample type" = "type",
                                  "Diagnosis"   = "Immunodeficiency_diagnosis"))
        ),
        width = 250
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Boxplot Comparison"),
          plotlyOutput("alpha_boxplot", height = "500px")
        ),
        card(
          card_header("Per-Sample Diversity"),
          plotlyOutput("alpha_sample_plot", height = "500px")
        )
      )
    )
  ),

  # ── Beta Diversity (PCoA) tab ──────────────────────────────────────────────
  nav_panel(
    title = "Beta Diversity",
    icon  = icon("project-diagram"),
    value = "beta_tab",
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        selectInput("beta_colour", "Colour by",
                    choices = c("Sample type" = "type",
                                "Diagnosis"   = "Immunodeficiency_diagnosis")),
        width = 250
      ),
      card(
        card_header("PCoA — Bray-Curtis Dissimilarity"),
        plotlyOutput("beta_pcoa", height = "600px")
      ),
      card(
        card_header("PERMANOVA Results"),
        verbatimTextOutput("beta_permanova")
      )
    )
  ),

  # ── Taxonomic Profiles tab ─────────────────────────────────────────────────
  nav_panel(
    title = "Taxonomy",
    icon  = icon("bacteria"),
    value = "taxonomy_tab",
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        conditionalPanel(
          condition = "output.is_clinician == true",
          selectInput("tax_diagnosis", "Diagnosis",
                      choices = c("All", "CVID", "XLA", "HG", "SID"),
                      selected = "CVID")
        ),
        selectInput("tax_type", "Sample Type",
                    choices = c("All", "Exhale", "Sputum")),
        sliderInput("tax_top_n", "Top N genera", min = 10, max = 50,
                    value = 30, step = 5),
        width = 250
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Relative Abundance — Stacked Bar Chart",
          actionButton("expand_tax_bar", label = NULL, icon = icon("expand"),
                       class = "btn btn-sm btn-outline-light")
        ),
        plotlyOutput("tax_bar", height = "600px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Genus Total Counts"),
          plotlyOutput("tax_counts", height = "500px")
        ),
        card(
          card_header("Genus Prevalence"),
          plotlyOutput("tax_prevalence", height = "500px")
        )
      )
    )
  ),

  # ── Pathogenomics Heatmap tab ──────────────────────────────────────────────
  nav_panel(
    title = "Pathogenomics",
    icon  = icon("disease"),
    value = "patho_tab",
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        selectInput("patho_db", "Database",
                    choices = c("All", "ARGs (CARD)" = "card",
                                "Virulence (VFDB)" = "vfdb",
                                "MGEs (PlasmidFinder)" = "plasmidfinder")),
        sliderInput("patho_min_tpm", "Min TPM threshold",
                    min = 0, max = 500, value = 10, step = 10),
        width = 250
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Gene Presence / Abundance Heatmap",
          actionButton("expand_patho_heatmap", label = NULL, icon = icon("expand"),
                       class = "btn btn-sm btn-outline-light")
        ),
        plotOutput("patho_heatmap", height = "700px")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Alpha Diversity of Genetic Elements"),
          plotlyOutput("patho_alpha", height = "400px")
        ),
        card(
          card_header("Gene Summary Table"),
          DTOutput("patho_table")
        )
      )
    )
  ),

  # ── Summary Report tab ──────────────────────────────────────────────────────
  nav_panel(
    title = "Summary Report",
    icon  = icon("file-medical"),
    value = "summary_tab",
    layout_sidebar(
      sidebar = sidebar(
        title = "Options",
        sliderInput("summary_top_n", "Top N species", min = 3, max = 20,
                    value = 5, step = 1),
        conditionalPanel(
          condition = "output.is_clinician == true",
          selectInput("summary_filter_id", "Research ID",
                      choices = c("All" = "All", valid_patient_ids))
        ),
        width = 250
      ),
      card(
        card_header("Top Species & Drug Resistance Profile"),
        DTOutput("summary_species_table")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Species Abundance"),
          plotlyOutput("summary_species_bar", height = "450px")
        ),
        card(
          card_header("Drug Resistance Classes"),
          plotlyOutput("summary_drug_bar", height = "450px")
        )
      )
    )
  ),

  # ── Nav spacer + footer ────────────────────────────────────────────────────
  nav_spacer(),
  nav_item(
    tags$span(style = "color:#2EC4B6; font-size:0.8em;",
              "E-Breathomics v0.1")
  )
)
