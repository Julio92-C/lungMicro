# server.R — E-Breathomics reactive logic

function(input, output, session) {

  # ══════════════════════════════════════════════════════════════════════════

  # LOGIN GATE
  # ══════════════════════════════════════════════════════════════════════════
  logged_in <- reactiveVal(FALSE)

  observeEvent(input$login_button, {
    req(input$user_id_field)
    if (input$user_id_field %in% as.character(valid_patient_ids)) {
      logged_in(TRUE)
      nav_select("main_nav", selected = "overview_tab")
    } else {
      showNotification("Invalid Research ID", type = "error")
    }
  })

  # Reactive: selected patient data
  patient <- reactive({
    req(logged_in())
    patient_metadata %>% filter(ID == as.numeric(input$user_id_field))
  })

  patient_samples <- reactive({
    req(logged_in())
    sample_metadata %>% filter(ID == as.numeric(input$user_id_field))
  })

  # ══════════════════════════════════════════════════════════════════════════
  # OVERVIEW TAB
  # ══════════════════════════════════════════════════════════════════════════
  output$ov_id    <- renderText({ req(patient()); as.character(patient()$ID[1]) })
  output$ov_diag  <- renderText({ req(patient()); patient()$Immunodeficiency_diagnosis[1] })
  output$ov_treat <- renderText({ req(patient()); patient()$Treatment[1] })

  output$ov_table <- renderDT({
    req(patient())
    patient() %>%
      select(ID, Age, Sex, Immunodeficiency_diagnosis, Treatment,
             Immunocompromised_type) %>%
      distinct() %>%
      datatable(options = list(dom = "t", paging = FALSE),
                rownames = FALSE, class = "compact stripe")
  })

  output$ov_samples <- renderDT({
    req(patient_samples())
    patient_samples() %>%
      datatable(options = list(dom = "t", paging = FALSE),
                rownames = FALSE, class = "compact stripe")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # ALPHA DIVERSITY TAB
  # ══════════════════════════════════════════════════════════════════════════
  alpha_data <- reactive({
    # Build species abundance matrix
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

    data.frame(
      sample   = rownames(abundance_matrix),
      Richness = as.numeric(specnumber(abundance_matrix)),
      Shannon  = as.numeric(diversity(abundance_matrix, index = "shannon"))
    ) %>%
      inner_join(
        bracken_merged %>%
          select(sample, ID, Sex, type, Immunodeficiency_diagnosis) %>%
          distinct(),
        by = "sample"
      )
  })

  output$alpha_boxplot <- renderPlotly({
    df <- alpha_data()
    idx  <- input$alpha_index
    grp  <- input$alpha_group
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
    grp <- input$alpha_group
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
          select(sample, type, Immunodeficiency_diagnosis) %>% distinct(),
        by = "sample"
      )

    list(pcoa_df = pcoa_df, var_expl = var_expl,
         bc_dist = bc_dist, meta = pcoa_df)
  })

  output$beta_pcoa <- renderPlotly({
    bd  <- beta_data()
    df  <- bd$pcoa_df
    ve  <- bd$var_expl
    col <- input$beta_colour
    pal <- if (col == "type") sample_palette else ip_palette

    p <- ggplot(df, aes(x = PC1, y = PC2, colour = .data[[col]], label = sample)) +
      geom_point(size = 3) +
      stat_ellipse(lwd = 0.8, linetype = "dashed") +
      scale_color_manual(values = pal) +
      labs(
        x     = paste0("PC1 (", round(ve[1], 2), "%)"),
        y     = paste0("PC2 (", round(ve[2], 2), "%)"),
        color = col
      ) +
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
    df <- genetable_normdata %>%
      filter(TPM >= input$patho_min_tpm)

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
}
