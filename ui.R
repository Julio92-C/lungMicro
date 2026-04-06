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
          selectInput("user_id_field", "Research ID",
                      choices = c("Select your ID" = "", valid_patient_ids)),
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
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Patient ID",    textOutput("ov_id"),   showcase = icon("id-card"),
                theme = "primary"),
      value_box("Diagnosis",     textOutput("ov_diag"), showcase = icon("stethoscope"),
                theme = "secondary"),
      value_box("Treatment",     textOutput("ov_treat"), showcase = icon("pills"),
                theme = "info")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Patient Details"),
        DTOutput("ov_table")
      ),
      card(
        card_header("Samples Available"),
        DTOutput("ov_samples")
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
        selectInput("alpha_group", "Group by",
                    choices = c("Sample type" = "type",
                                "Diagnosis"   = "Immunodeficiency_diagnosis")),
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
        selectInput("tax_type", "Sample Type",
                    choices = c("All", "Exhale", "Sputum")),
        sliderInput("tax_top_n", "Top N genera", min = 10, max = 50,
                    value = 30, step = 5),
        width = 250
      ),
      card(
        card_header("Relative Abundance — Stacked Bar Chart"),
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
        card_header("Gene Presence / Abundance Heatmap"),
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

  # ── Nav spacer + footer ────────────────────────────────────────────────────
  nav_spacer(),
  nav_item(
    tags$span(style = "color:#2EC4B6; font-size:0.8em;",
              "E-Breathomics v0.1")
  )
)
