# server.R — E-Breathomics reactive logic

function(input, output, session) {

  # ══════════════════════════════════════════════════════════════════════════

  # LOGIN GATE
  # ══════════════════════════════════════════════════════════════════════════
  logged_in  <- reactiveVal(FALSE)
  user_role  <- reactiveVal(NULL)   # "patient" or "clinician"
  user_id    <- reactiveVal(NULL)   # Research ID (character) or NULL for clinician

  observeEvent(input$login_button, {
    if (input$login_role == "Patient") {
      req(input$user_id_field)
      if (input$user_id_field %in% as.character(valid_patient_ids)) {
        logged_in(TRUE)
        user_role("patient")
        user_id(input$user_id_field)
        nav_select("main_nav", selected = "overview_tab")
      } else {
        showNotification("Invalid Research ID", type = "error")
      }
    } else {
      # Clinician login
      if (identical(input$clinician_password, CLINICIAN_CODE)) {
        logged_in(TRUE)
        user_role("clinician")
        user_id(NULL)
        nav_select("main_nav", selected = "overview_tab")
      } else {
        showNotification("Invalid clinician code", type = "error")
      }
    }
  })

  # Expose role to conditionalPanel in UI
  output$is_clinician <- reactive({ identical(user_role(), "clinician") })
  outputOptions(output, "is_clinician", suspendWhenHidden = FALSE)

  # Set patient-friendly defaults after login
  observeEvent(user_role(), {
    if (user_role() == "patient") {
      updateSliderInput(session, "tax_top_n", value = 20)
    }
  })

  # Reactive: the active Research ID (patient's own ID; clinicians get "All")
  active_id <- reactive({
    req(logged_in())
    if (user_role() == "patient") {
      user_id()
    } else {
      "All"
    }
  })

  # Reactive: Overview-specific Research ID (clinician filter is local to Overview)
  overview_id <- reactive({
    req(logged_in())
    if (user_role() == "patient") {
      user_id()
    } else {
      input$clinician_filter_id
    }
  })

  # Reactive: selected patient data (Overview tab)
  patient <- reactive({
    req(logged_in())
    rid <- overview_id()
    if (is.null(rid) || rid == "All") return(patient_metadata)
    patient_metadata %>% filter(ID == as.numeric(rid))
  })

  patient_samples <- reactive({
    req(logged_in())
    rid <- overview_id()
    if (is.null(rid) || rid == "All") return(sample_metadata)
    sample_metadata %>% filter(ID == as.numeric(rid))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # OVERVIEW TAB
  # ══════════════════════════════════════════════════════════════════════════
  output$ov_id_card <- renderUI({
    req(patient())
    rid <- overview_id()
    if (is.null(rid) || rid == "All") {
      tags$span("All Patients")
    } else {
      n_samples <- nrow(patient_samples())
      tags$div(
        tags$div(as.character(patient()$ID[1])),
        tags$div(style = "font-size:0.75em; opacity:0.8; margin-top:4px;",
                 paste0(n_samples, " sample", ifelse(n_samples != 1, "s", "")))
      )
    }
  })
  output$ov_diag_card <- renderUI({
    req(patient())
    rid <- overview_id()
    if (is.null(rid) || rid == "All") {
      tags$span("—")
    } else {
      pat <- patient()
      tags$div(
        tags$div(pat$Immunodeficiency_diagnosis[1]),
        tags$div(style = "font-size:0.75em; opacity:0.8; margin-top:4px;",
                 paste0(pat$Age[1], " yrs | ", pat$Sex[1]))
      )
    }
  })
  output$ov_treat <- renderText({
    req(patient())
    rid <- overview_id()
    if (is.null(rid) || rid == "All") "—" else patient()$Treatment[1]
  })

  output$ov_table <- renderDT({
    req(patient())
    patient() %>%
      select(ID, Age, Sex, Immunodeficiency_diagnosis, Treatment,
             Immunocompromised_type) %>%
      distinct() %>%
      datatable(options = list(dom = "t", paging = FALSE),
                rownames = FALSE, class = "compact stripe")
  })

  # Clinician: samples table
  output$ov_samples_clin <- renderDT({
    req(patient_samples())
    patient_samples() %>%
      datatable(options = list(dom = "t", paging = FALSE),
                rownames = FALSE, class = "compact stripe")
  })

  # ── Patient Overview: GE pie chart ──────────────────────────────────────────
  output$ov_ge_pie <- renderPlotly({
    req(user_role() == "patient")
    rid <- active_id()
    req(!is.null(rid) && rid != "All")

    ge_counts <- genetable_normdata %>%
      filter(ID == as.numeric(rid)) %>%
      group_by(DATABASE) %>%
      summarise(n_genes = n_distinct(GENE), .groups = "drop") %>%
      mutate(label = case_when(
        DATABASE == "card"           ~ "ARGs (CARD)",
        DATABASE == "vfdb"           ~ "Virulence (VFDB)",
        DATABASE == "plasmidfinder"  ~ "MGEs (PlasmidFinder)",
        TRUE                         ~ DATABASE
      ))

    db_colors <- c("ARGs (CARD)" = "#f2615a", "Virulence (VFDB)" = "#8E44AD",
                    "MGEs (PlasmidFinder)" = "#2EC4B6")

    plot_ly(ge_counts, labels = ~label, values = ~n_genes,
            type = "pie",
            marker = list(colors = db_colors[ge_counts$label]),
            textinfo = "label+value",
            textfont = list(color = "#FAF0E6", size = 12),
            hoverinfo = "label+value+percent") %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        showlegend    = FALSE,
        font = list(color = "#FAF0E6")
      )
  })

  # ── Patient Overview: Top 5 taxa carrying GEs ──────────────────────────────
  output$ov_top_taxa_list <- renderUI({
    req(user_role() == "patient")
    rid <- active_id()
    req(!is.null(rid) && rid != "All")

    # Top 5 species by number of distinct genetic elements
    top_taxa <- abri_kraken2Bracken %>%
      filter(ID == as.numeric(rid)) %>%
      group_by(name) %>%
      summarise(
        n_GEs     = n_distinct(GENE),
        databases = paste(sort(unique(DATABASE)), collapse = ", "),
        .groups   = "drop"
      ) %>%
      arrange(desc(n_GEs)) %>%
      slice_head(n = 5) %>%
      mutate(short_name = str_replace(name, "^(\\w)\\w+\\s", "\\1. "))

    db_badge <- function(db_str) {
      dbs <- str_split(db_str, ", ")[[1]]
      colors <- c(card = "#f2615a", vfdb = "#8E44AD", plasmidfinder = "#2EC4B6")
      labels <- c(card = "CARD", vfdb = "VFDB", plasmidfinder = "PF")
      lapply(dbs, function(d) {
        tags$span(
          style = paste0("background:", colors[[d]], "; color:#fff; padding:1px 6px;",
                         " border-radius:4px; font-size:0.7em; margin-left:4px;"),
          labels[[d]]
        )
      })
    }

    items <- lapply(seq_len(nrow(top_taxa)), function(i) {
      row <- top_taxa[i, ]
      tags$div(
        style = "display:flex; align-items:center; justify-content:space-between; padding:6px 0; border-bottom:1px solid #1a2f4e;",
        tags$span(
          style = "font-style:italic; color:#FAF0E6;",
          paste0(i, ". ", row$short_name)
        ),
        tags$span(
          style = "display:flex; align-items:center; gap:4px;",
          tags$span(
            style = "background:#2EC4B6; color:#0b172a; padding:1px 8px; border-radius:10px; font-size:0.75em; font-weight:600;",
            paste0(row$n_GEs, " GEs")
          ),
          db_badge(row$databases)
        )
      )
    })

    do.call(tags$div, items)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # SUMMARY REPORT TAB (both profiles)
  # ══════════════════════════════════════════════════════════════════════════
  summary_data <- reactive({
    # Determine active ID for this tab
    if (user_role() == "patient") {
      rid <- user_id()
    } else {
      rid <- input$summary_filter_id
    }

    src <- abri_kraken2Bracken
    if (!is.null(rid) && rid != "All") {
      src <- src %>% filter(ID == as.numeric(rid))
    }

    top_n_val <- input$summary_top_n

    # Top N species by total Bracken count
    top_species <- src %>%
      group_by(name) %>%
      summarise(Total_Count = sum(TotalBCount, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total_Count)) %>%
      slice_head(n = top_n_val)

    # Drug resistance from CARD hits for those species
    resistance_df <- src %>%
      filter(name %in% top_species$name, DATABASE == "card") %>%
      group_by(name) %>%
      summarise(
        Drug_Resistance = paste(
          sort(unique(na.omit(unlist(str_split(RESISTANCE, ";"))))),
          collapse = ", "
        ),
        n_ARGs = n_distinct(GENE),
        .groups = "drop"
      )

    result <- top_species %>%
      left_join(resistance_df, by = "name") %>%
      mutate(
        Drug_Resistance = ifelse(is.na(Drug_Resistance) | Drug_Resistance == "",
                                 "No ARGs detected", Drug_Resistance),
        n_ARGs = ifelse(is.na(n_ARGs), 0, n_ARGs),
        Species = str_replace(name, "^(\\w)\\w+\\s", "\\1. ")
      )

    list(table = result, source = src)
  })

  output$summary_species_table <- renderDT({
    df <- summary_data()$table %>%
      select(Species, Count = Total_Count, ARGs = n_ARGs,
             `Drug Resistance Classes` = Drug_Resistance)

    datatable(df,
              options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                             columnDefs = list(list(width = "40%", targets = 3))),
              rownames = FALSE, class = "compact stripe")
  })

  output$summary_species_bar <- renderPlotly({
    df <- summary_data()$table

    p <- ggplot(df, aes(x = reorder(Species, Total_Count), y = Total_Count,
                        fill = Species)) +
      geom_col(width = 0.6) +
      scale_y_log10() +
      coord_flip() +
      labs(x = NULL, y = "Total Count (log10)") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "none",
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6")
      )

    ggplotly(p, tooltip = c("y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  output$summary_drug_bar <- renderPlotly({
    df <- summary_data()$source

    # Count unique ARGs per drug resistance class
    drug_df <- df %>%
      filter(DATABASE == "card", !is.na(RESISTANCE)) %>%
      mutate(drug_class = str_split(RESISTANCE, ";")) %>%
      tidyr::unnest(drug_class) %>%
      group_by(drug_class) %>%
      summarise(n_genes = n_distinct(GENE), .groups = "drop") %>%
      arrange(desc(n_genes))

    req(nrow(drug_df) > 0)

    p <- ggplot(drug_df, aes(x = reorder(drug_class, n_genes), y = n_genes)) +
      geom_col(fill = "#f2615a", width = 0.6) +
      coord_flip() +
      labs(x = NULL, y = "Number of ARGs") +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "none",
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6")
      )

    ggplotly(p, tooltip = c("y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # PATIENT PDF REPORT DOWNLOAD
  # ══════════════════════════════════════════════════════════════════════════
  output$download_patient_pdf <- downloadHandler(
    filename = function() {
      paste0("microbiome_report_", user_id(), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      withProgress(message = "Generating your report...", value = 0.1, {

        rid <- user_id()
        rid_num <- as.numeric(rid)

        # 1. Patient info
        pat <- patient_metadata %>% filter(ID == rid_num) %>% distinct(ID, .keep_all = TRUE)
        pat_samples <- sample_metadata %>% filter(ID == rid_num)

        patient_info <- list(
          id           = rid,
          age          = pat$Age[1],
          sex          = pat$Sex[1],
          diagnosis    = pat$Immunodeficiency_diagnosis[1],
          treatment    = pat$Treatment[1],
          n_samples    = nrow(pat_samples),
          sample_types = paste(unique(pat_samples$type), collapse = ", ")
        )

        setProgress(0.3)

        # 2. Top species
        src <- abri_kraken2Bracken %>% filter(ID == rid_num)
        top_species_df <- src %>%
          group_by(name) %>%
          summarise(Total_Count = sum(TotalBCount, na.rm = TRUE), .groups = "drop") %>%
          arrange(desc(Total_Count)) %>%
          slice_head(n = 10) %>%
          mutate(Species = str_replace(name, "^(\\w)\\w+\\s", "\\1. "))

        setProgress(0.4)

        # 3. Drug resistance
        drug_resistance_df <- src %>%
          filter(DATABASE == "card", !is.na(RESISTANCE)) %>%
          group_by(name) %>%
          summarise(
            n_ARGs = n_distinct(GENE),
            Drug_Classes = paste(sort(unique(na.omit(unlist(str_split(RESISTANCE, ";"))))),
                                 collapse = ", "),
            .groups = "drop"
          ) %>%
          mutate(Species = str_replace(name, "^(\\w)\\w+\\s", "\\1. "))

        setProgress(0.5)

        # 4. Alpha diversity
        base_df <- bracken_merged %>%
          filter(ID == rid_num, type != "Control", sample != "EB-42D") %>%
          filter(!grepl("Terrabacteria group|Bacteroidota/Chlorobiota group|FCB group", name)) %>%
          filter(str_count(name, "\\S+") > 1)

        if (nrow(base_df) > 0) {
          mat_df <- base_df %>%
            select(sample, name, log_count) %>%
            pivot_wider(names_from = name, values_from = log_count,
                        values_fill = 0, values_fn = sum) %>%
            distinct(sample, .keep_all = TRUE) %>%
            column_to_rownames("sample")
          abundance_matrix <- as.matrix(mat_df)
          alpha_diversity_df <- data.frame(
            Sample   = rownames(abundance_matrix),
            Richness = as.numeric(vegan::specnumber(abundance_matrix)),
            Shannon  = round(as.numeric(vegan::diversity(abundance_matrix,
                                                         index = "shannon")), 2)
          ) %>%
            inner_join(base_df %>% select(sample, type) %>% distinct(),
                       by = c("Sample" = "sample"))
        } else {
          alpha_diversity_df <- data.frame(Sample = character(), Richness = numeric(),
                                           Shannon = numeric(), type = character())
        }

        setProgress(0.6)

        # 5. Genetic elements summary
        ge_summary_df <- genetable_normdata %>%
          filter(ID == rid_num) %>%
          group_by(DATABASE) %>%
          summarise(n_genes = n_distinct(GENE), .groups = "drop") %>%
          mutate(Category = case_when(
            DATABASE == "card"          ~ "Antibiotic Resistance Genes",
            DATABASE == "vfdb"          ~ "Virulence Factors",
            DATABASE == "plasmidfinder" ~ "Mobile Genetic Elements",
            TRUE                        ~ DATABASE
          ))

        setProgress(0.8)

        # 6. Render Rmd in tempdir to avoid OneDrive locking
        temp_rmd <- file.path(tempdir(), "patient_report.Rmd")
        file.copy("report_templates/patient_report.Rmd", temp_rmd, overwrite = TRUE)

        rmarkdown::render(
          input       = temp_rmd,
          output_file = file,
          params = list(
            patient_id        = rid,
            patient_info       = patient_info,
            top_species_df     = top_species_df,
            drug_resistance_df = drug_resistance_df,
            alpha_diversity_df = alpha_diversity_df,
            ge_summary_df      = ge_summary_df,
            report_date        = format(Sys.Date(), "%d %B %Y")
          ),
          envir = new.env(parent = globalenv())
        )

        setProgress(1)
      })
    }
  )

  # ══════════════════════════════════════════════════════════════════════════
  # ALPHA DIVERSITY TAB
  # ══════════════════════════════════════════════════════════════════════════
  alpha_data <- reactive({
    rid <- active_id()

    base_df <- bracken_merged %>%
      filter(type != "Control", sample != "EB-42D") %>%
      filter(!grepl("Terrabacteria group|Bacteroidota/Chlorobiota group|FCB group", name)) %>%
      filter(str_count(name, "\\S+") > 1)

    # Filter to patient's samples when applicable
    if (!is.null(rid) && rid != "All") {
      base_df <- base_df %>% filter(ID == as.numeric(rid))
    }

    mat_df <- base_df %>%
      select(sample, name, log_count) %>%
      group_by(sample) %>%
      pivot_wider(names_from = name, values_from = log_count,
                  values_fill = 0, values_fn = sum) %>%
      distinct(sample, .keep_all = TRUE) %>%
      column_to_rownames("sample")

    abundance_matrix <- as.matrix(mat_df)

    data.frame(
      sample   = rownames(abundance_matrix),
      Richness = as.numeric(specnumber(abundance_matrix)),
      Shannon  = as.numeric(diversity(abundance_matrix, index = "shannon"))
    ) %>%
      inner_join(
        base_df %>%
          select(sample, ID, Sex, type, Immunodeficiency_diagnosis) %>%
          distinct(),
        by = "sample"
      )
  })

  output$alpha_boxplot <- renderPlotly({
    df <- alpha_data()
    idx  <- input$alpha_index
    grp  <- if (user_role() == "patient") "type" else input$alpha_group
    pal  <- if (grp == "type") sample_palette else ip_palette

    kw <- kruskal.test(reformulate(grp, idx), data = df)
    label_kw <- paste("Kruskal-Wallis p =", format(kw$p.value, digits = 3))

    p <- ggplot(df, aes(x = .data[[grp]], y = .data[[idx]], fill = .data[[grp]])) +
      geom_boxplot(width = 0.5) +
      scale_fill_manual(values = pal) +
      labs(x = grp, y = idx) +
      annotate("text", x = Inf, y = Inf, label = label_kw,
               hjust = 1.1, vjust = 1.5, size = 4, color = "#FAF0E6") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6")
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  output$alpha_sample_plot <- renderPlotly({
    df  <- alpha_data()
    idx <- input$alpha_index
    grp <- if (user_role() == "patient") "type" else input$alpha_group
    pal <- if (grp == "type") sample_palette else ip_palette

    p <- ggplot(df, aes(x = sample, y = .data[[idx]], fill = .data[[grp]])) +
      geom_point(aes(colour = .data[[grp]]), size = 2) +
      geom_line(aes(group = 1), colour = "#FAF0E6", linewidth = 0.3) +
      geom_hline(yintercept = mean(df[[idx]]), linetype = "dashed", colour = "red") +
      scale_colour_manual(values = pal) +
      labs(x = "Sample", y = idx) +
      facet_wrap(as.formula(paste("~", grp)), scales = "free_x", nrow = 2) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position  = "none",
        axis.text.x      = element_text(angle = 45, hjust = 1, colour = "#FAF0E6"),
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6"),
        strip.text = element_text(colour = "#FAF0E6")
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # BETA DIVERSITY (PCoA) TAB
  # ══════════════════════════════════════════════════════════════════════════
  beta_data <- reactive({
    rid <- active_id()

    # Always compute PCoA on the full cohort
    mat_df <- bracken_merged %>%
      filter(type != "Control", sample != "EB-42D") %>%
      filter(!grepl("Terrabacteria group|Bacteroidota/Chlorobiota group|FCB group", name)) %>%
      filter(str_count(name, "\\S+") > 1) %>%
      select(sample, name, log_count) %>%
      group_by(sample) %>%
      pivot_wider(names_from = name, values_from = log_count,
                  values_fill = 0, values_fn = sum) %>%
      distinct(sample, .keep_all = TRUE) %>%
      column_to_rownames("sample")

    abundance_matrix <- as.matrix(mat_df)
    rel_matrix <- decostand(abundance_matrix, method = "hellinger")
    clean_matrix <- na.omit(rel_matrix)

    bc_dist    <- vegdist(clean_matrix, method = "bray")
    pcoa_res   <- cmdscale(bc_dist, eig = TRUE, k = 2)
    eig_vals   <- pcoa_res$eig
    var_expl   <- eig_vals / sum(eig_vals) * 100

    pcoa_df <- data.frame(
      sample = rownames(clean_matrix),
      PC1    = pcoa_res$points[, 1],
      PC2    = pcoa_res$points[, 2]
    ) %>%
      inner_join(
        bracken_merged %>%
          select(sample, ID, type, Immunodeficiency_diagnosis) %>% distinct(),
        by = "sample"
      )

    # Flag patient's own samples for highlighting
    if (!is.null(rid) && rid != "All") {
      pcoa_df <- pcoa_df %>%
        mutate(highlight = ID == as.numeric(rid))
    } else {
      pcoa_df <- pcoa_df %>% mutate(highlight = FALSE)
    }

    list(pcoa_df = pcoa_df, var_expl = var_expl,
         bc_dist = bc_dist, meta = pcoa_df)
  })

  output$beta_pcoa <- renderPlotly({
    bd  <- beta_data()
    df  <- bd$pcoa_df
    ve  <- bd$var_expl
    col <- input$beta_colour
    pal <- if (col == "type") sample_palette else ip_palette

    has_highlight <- any(df$highlight)

    if (has_highlight) {
      # Patient view: show full cohort, highlight patient's points
      df <- df %>% mutate(
        pt_size  = ifelse(highlight, 5, 2),
        pt_alpha = ifelse(highlight, 1, 0.3)
      )
      p <- ggplot(df, aes(x = PC1, y = PC2, colour = .data[[col]], label = sample)) +
        geom_point(aes(size = pt_size, alpha = pt_alpha)) +
        scale_size_identity() +
        scale_alpha_identity() +
        stat_ellipse(lwd = 0.8, linetype = "dashed") +
        scale_color_manual(values = pal) +
        labs(
          x     = paste0("PC1 (", round(ve[1], 2), "%)"),
          y     = paste0("PC2 (", round(ve[2], 2), "%)"),
          color = col
        )
    } else {
      p <- ggplot(df, aes(x = PC1, y = PC2, colour = .data[[col]], label = sample)) +
        geom_point(size = 3) +
        stat_ellipse(lwd = 0.8, linetype = "dashed") +
        scale_color_manual(values = pal) +
        labs(
          x     = paste0("PC1 (", round(ve[1], 2), "%)"),
          y     = paste0("PC2 (", round(ve[2], 2), "%)"),
          color = col
        )
    }

    p <- p +
      theme_minimal(base_size = 14) +
      theme(
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6"),
        legend.text = element_text(colour = "#FAF0E6")
      )

    ggplotly(p, tooltip = c("label", "x", "y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  output$beta_permanova <- renderPrint({
    bd  <- beta_data()
    col <- input$beta_colour
    meta_df <- bd$pcoa_df %>%
      distinct(sample, .keep_all = TRUE) %>%
      column_to_rownames("sample")

    perm <- adonis2(
      reformulate(col, "bd$bc_dist"),
      data = meta_df, permutations = 999, method = "bray"
    )
    print(perm)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # TAXONOMIC PROFILES TAB
  # ══════════════════════════════════════════════════════════════════════════
  tax_data <- reactive({
    rid <- active_id()

    df <- bracken_merged %>%
      filter(type != "Control", sample != "EB-42D") %>%
      filter(!grepl("Terrabacteria group|Bacteroidota/Chlorobiota group|FCB group|Cyanobacteriota/Melainabacteria group|Enterobacteriaceae|Bacillota|Klebsiella/Raoultella group", name)) %>%
      filter(str_count(name, "\\S+") > 1) %>%
      mutate(genus = str_extract(name, "^\\w+")) %>%
      filter(count > 1) %>%
      group_by(sample) %>%
      mutate(log_count = log(count + 1),
             Percentage = (count / sum(count)) * 100) %>%
      ungroup()

    # Filter to patient's samples when applicable
    if (!is.null(rid) && rid != "All") {
      df <- df %>% filter(ID == as.numeric(rid))
    }

    # Clinician diagnosis filter
    if (identical(user_role(), "clinician") &&
        !is.null(input$tax_diagnosis) && input$tax_diagnosis != "All") {
      df <- df %>% filter(Immunodeficiency_diagnosis == input$tax_diagnosis)
    }

    if (input$tax_type != "All") {
      df <- df %>% filter(type == input$tax_type)
    }

    top_genera <- df %>%
      group_by(genus) %>%
      summarise(total = sum(count, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice_head(n = input$tax_top_n) %>%
      pull(genus)

    df %>%
      mutate(genus = ifelse(genus %in% top_genera, genus, "Others")) %>%
      group_by(sample, genus, type, Immunodeficiency_diagnosis, ID, Sex) %>%
      summarise(Percentage = sum(Percentage), count = sum(count), .groups = "drop")
  })

  output$tax_bar <- renderPlotly({
    df <- tax_data()
    n_genera <- length(unique(df$genus))
    pal <- paletteer_d("ggsci::default_igv", n = min(n_genera, 51))

    p <- ggplot(df, aes(x = sample, y = Percentage, fill = genus)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Immunodeficiency_diagnosis + ID + Sex,
                 scales = "free_x", space = "free", switch = "x") +
      scale_fill_manual(values = pal) +
      labs(x = "Sample", y = "Relative Abundance (%)", fill = "Genus") +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x      = element_text(angle = 45, hjust = 1, colour = "#FAF0E6"),
        legend.position   = "bottom",
        legend.key.size   = unit(0.3, "cm"),
        plot.background   = element_rect(fill = "transparent", colour = NA),
        panel.background  = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6"),
        strip.text = element_text(colour = "#FAF0E6", size = 8),
        legend.text = element_text(colour = "#FAF0E6", size = 7)
      ) +
      guides(fill = guide_legend(nrow = 4))

    ggplotly(p, tooltip = c("fill", "y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent",
             legend = list(font = list(size = 8)))
  })

  output$tax_counts <- renderPlotly({
    df <- tax_data() %>%
      filter(genus != "Others") %>%
      group_by(genus) %>%
      summarise(totCount = sum(count), .groups = "drop") %>%
      arrange(desc(totCount))

    n_genera <- length(unique(df$genus))
    pal <- paletteer_d("ggsci::default_igv", n = min(n_genera, 51))

    p <- ggplot(df, aes(x = reorder(genus, totCount), y = totCount, fill = genus)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = pal) +
      scale_y_log10() +
      labs(x = "Genus", y = "Total Count (log10)") +
      coord_flip() +
      theme_minimal(base_size = 11) +
      theme(
        legend.position  = "none",
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6", size = 7)
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  output$tax_prevalence <- renderPlotly({
    df <- tax_data() %>%
      filter(genus != "Others") %>%
      group_by(genus) %>%
      summarise(SampleCount = n_distinct(sample), .groups = "drop") %>%
      arrange(desc(SampleCount))

    n_genera <- length(unique(df$genus))
    pal <- paletteer_d("ggsci::default_igv", n = min(n_genera, 51))

    p <- ggplot(df, aes(x = SampleCount, y = reorder(genus, SampleCount), fill = genus)) +
      geom_bar(stat = "identity") +
      geom_vline(xintercept = median(df$SampleCount), linetype = "dashed",
                 colour = "#FAF0E6") +
      scale_fill_manual(values = pal) +
      labs(x = "Sample Count", y = "") +
      theme_minimal(base_size = 11) +
      theme(
        legend.position  = "none",
        plot.background  = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        text  = element_text(colour = "#FAF0E6"),
        axis.text = element_text(colour = "#FAF0E6", size = 7)
      )

    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # PATHOGENOMICS TAB
  # ══════════════════════════════════════════════════════════════════════════
  patho_filtered <- reactive({
    rid <- active_id()

    df <- genetable_normdata %>%
      filter(TPM >= input$patho_min_tpm)

    # Filter to patient's samples when applicable
    if (!is.null(rid) && rid != "All") {
      df <- df %>% filter(ID == as.numeric(rid))
    }

    if (input$patho_db != "All") {
      df <- df %>% filter(DATABASE == input$patho_db)
    }
    df
  })

  output$patho_heatmap <- renderPlot({
    df <- patho_filtered()
    req(nrow(df) > 0)

    mat_df <- df %>%
      mutate(log_tpm = log(TPM + 1)) %>%
      select(sample, GENE, log_tpm) %>%
      pivot_wider(names_from = sample, values_from = log_tpm,
                  values_fill = 0, values_fn = sum) %>%
      distinct(GENE, .keep_all = TRUE) %>%
      column_to_rownames("GENE")

    mat <- as.matrix(mat_df)

    # Sort by row sums
    mat <- mat[order(rowSums(mat), decreasing = TRUE), , drop = FALSE]

    # Limit to top 60 genes for readability
    if (nrow(mat) > 60) mat <- mat[1:60, , drop = FALSE]

    # Annotation columns
    ann_df <- df %>%
      select(sample, type, Immunodeficiency_diagnosis) %>%
      distinct() %>%
      filter(sample %in% colnames(mat)) %>%
      column_to_rownames("sample")

    ann_colors <- list(
      type = sample_palette,
      Immunodeficiency_diagnosis = ip_palette
    )

    pheatmap(
      mat,
      cluster_cols   = FALSE,
      cluster_rows   = FALSE,
      scale          = "none",
      color          = colorRampPalette(c("#0b172a", "#2EC4B6", "#f2615a"))(50),
      border_color   = NA,
      annotation_col = ann_df,
      annotation_colors = ann_colors,
      fontsize_row   = 8,
      fontsize_col   = 9,
      fontsize       = 10,
      main           = paste("Gene Abundance (log TPM) —",
                             if (input$patho_db == "All") "All databases" else input$patho_db)
    )
  })

  output$patho_alpha <- renderPlotly({
    df <- patho_filtered()
    req(nrow(df) > 0)

    mat_df <- df %>%
      mutate(log_tpm = log(TPM + 1)) %>%
      select(sample, GENE, log_tpm) %>%
      pivot_wider(names_from = GENE, values_from = log_tpm,
                  values_fill = 0, values_fn = sum) %>%
      distinct(sample, .keep_all = TRUE) %>%
      column_to_rownames("sample")

    alpha_df <- data.frame(
      sample   = rownames(mat_df),
      Richness = as.numeric(specnumber(as.matrix(mat_df))),
      Shannon  = as.numeric(diversity(as.matrix(mat_df), index = "shannon"))
    ) %>%
      inner_join(
        df %>% select(sample, type, Immunodeficiency_diagnosis) %>% distinct(),
        by = "sample"
      )

    if (user_role() == "patient") {
      # Per-sample bar chart for patient view
      p <- ggplot(alpha_df, aes(x = sample, y = Shannon, fill = sample)) +
        geom_col(width = 0.6) +
        labs(x = "Sample", y = "Shannon Index") +
        theme_minimal(base_size = 13) +
        theme(
          legend.position  = "none",
          axis.text.x      = element_text(angle = 45, hjust = 1, colour = "#FAF0E6"),
          plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          text  = element_text(colour = "#FAF0E6"),
          axis.text = element_text(colour = "#FAF0E6")
        )
    } else {
      p <- ggplot(alpha_df, aes(x = Immunodeficiency_diagnosis, y = Shannon,
                                 fill = Immunodeficiency_diagnosis)) +
        geom_boxplot(width = 0.5) +
        scale_fill_manual(values = ip_palette) +
        labs(x = "Diagnosis", y = "Shannon Index") +
        theme_minimal(base_size = 13) +
        theme(
          legend.position  = "none",
          plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill = "transparent", colour = NA),
          text  = element_text(colour = "#FAF0E6"),
          axis.text = element_text(colour = "#FAF0E6")
        )
    }

    ggplotly(p) %>%
      layout(paper_bgcolor = "transparent", plot_bgcolor = "transparent")
  })

  output$patho_table <- renderDT({
    df <- patho_filtered()
    req(nrow(df) > 0)

    df %>%
      select(sample, GENE, DATABASE, RESISTANCE, TPM) %>%
      distinct() %>%
      arrange(desc(TPM)) %>%
      head(200) %>%
      datatable(
        options  = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        class    = "compact stripe"
      )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # EXPAND MODALS
  # ══════════════════════════════════════════════════════════════════════════
  observeEvent(input$expand_tax_bar, {
    showModal(modalDialog(
      title = "Relative Abundance — Stacked Bar Chart",
      plotlyOutput("tax_bar_modal", height = "80vh", width = "100%"),
      size  = "xl",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  output$tax_bar_modal <- renderPlotly({
    df <- tax_data()
    n_genera <- length(unique(df$genus))
    pal <- paletteer_d("ggsci::default_igv", n = min(n_genera, 51))

    p <- ggplot(df, aes(x = sample, y = Percentage, fill = genus)) +
      geom_bar(stat = "identity") +
      facet_grid(. ~ Immunodeficiency_diagnosis + ID + Sex,
                 scales = "free_x", space = "free", switch = "x") +
      scale_fill_manual(values = pal) +
      labs(x = "Sample", y = "Relative Abundance (%)", fill = "Genus") +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.position   = "bottom",
        legend.key.size   = unit(0.3, "cm")
      ) +
      guides(fill = guide_legend(nrow = 4))

    ggplotly(p, tooltip = c("fill", "y")) %>%
      layout(legend = list(font = list(size = 8)))
  })

  observeEvent(input$expand_patho_heatmap, {
    showModal(modalDialog(
      title = "Gene Presence / Abundance Heatmap",
      plotOutput("patho_heatmap_modal", height = "80vh", width = "100%"),
      size  = "xl",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  output$patho_heatmap_modal <- renderPlot({
    df <- patho_filtered()
    req(nrow(df) > 0)

    mat_df <- df %>%
      mutate(log_tpm = log(TPM + 1)) %>%
      select(sample, GENE, log_tpm) %>%
      pivot_wider(names_from = sample, values_from = log_tpm,
                  values_fill = 0, values_fn = sum) %>%
      distinct(GENE, .keep_all = TRUE) %>%
      column_to_rownames("GENE")

    mat <- as.matrix(mat_df)
    mat <- mat[order(rowSums(mat), decreasing = TRUE), , drop = FALSE]
    if (nrow(mat) > 60) mat <- mat[1:60, , drop = FALSE]

    ann_df <- df %>%
      select(sample, type, Immunodeficiency_diagnosis) %>%
      distinct() %>%
      filter(sample %in% colnames(mat)) %>%
      column_to_rownames("sample")

    ann_colors <- list(
      type = sample_palette,
      Immunodeficiency_diagnosis = ip_palette
    )

    pheatmap(
      mat,
      cluster_cols   = FALSE,
      cluster_rows   = FALSE,
      scale          = "none",
      color          = colorRampPalette(c("#0b172a", "#2EC4B6", "#f2615a"))(50),
      border_color   = NA,
      annotation_col = ann_df,
      annotation_colors = ann_colors,
      fontsize_row   = 9,
      fontsize_col   = 10,
      fontsize       = 11,
      main           = paste("Gene Abundance (log TPM) —",
                             if (input$patho_db == "All") "All databases" else input$patho_db)
    )
  })
}
