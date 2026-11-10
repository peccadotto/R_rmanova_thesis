# ==============================================================================
# REPEATED MEASURES COMPARISON
# ==============================================================================

# 1. CHECK AND INSTALL REQUIRED PACKAGES ---------------------------------------

  source("script/setup.R")

# 2. DATABASE IMPORT -----------------------------------------------------------

  source("script/import.R")

# 3. SAMPLE SIZE CALCULATION ---------------------------------------------------

  source("script/sample.R")

# 4. POWER CALCULATION ---------------------------------------------------------
  
  source("script/power.R")

# 5. STATISTICA DESCRITTIVA ----------------------------------------------------

  source("script/descriptive.R")
  # source("script/plots_descr.R")

# 5. START OF REPEATED MEASURES STATISTICAL ANALYSIS ---------------------------

  analyzed_vars <- unique(df_long$var)
  for (v in analyzed_vars) {
    df_sub <- df_long |> filter(var == v) |> drop_na(value)
    df_sub <- df_sub |>
      group_by(id, time) |>
      summarise(
        value = mean(value, na.rm = TRUE),
        .groups = "drop"
      )
    unique_timepoints <- sort(unique(df_sub$time))
    n_timepoints <- length(unique_timepoints)
    valid_ids <- df_sub |>
      group_by(id) |>
      summarise(
        N = n(),
        is_complete = all(unique_timepoints %in% time),
        .groups = "drop"
      ) |>
      filter(N == n_timepoints & is_complete) |>
      pull(id)
    df_sub <- df_sub |>
      filter(id %in% valid_ids) |>
      mutate(
        id = factor(id),
        time = factor(time, levels = unique_timepoints)
      )

  df_shapiro <- df_sub |>
    group_by(time) |>
    shapiro_test(value) |>
    data.frame() |>
    select(-any_of("var")) |>
    mutate(
      normal = ifelse(p > 0.05, "Yes (p > 0.05)", "No (p < 0.05)")
    )
  if (all(df_shapiro$p > 0.05)) {
    is_normal <- TRUE
  } else {
    is_normal <- FALSE
  }
  table_path <- file.path("export", paste0("df_shapiro_", v, ".rds"))
  saveRDS(df_shapiro, file = table_path)
  
  
  
  
  
  # 4. Parametric (ANOVA) vs Non-Parametric (Friedman) Branching
  if (is_normal) {
    # Normality RESPECTED --> ANOVA

    anova_ripetuta <- df_sub |>
      anova_test(dv = value, wid = id, within = time)
    print(anova_ripetuta)

    p_mauchly <- anova_ripetuta$`p.Mauchly`

    if (!is.null(p_mauchly) && p_mauchly < 0.05) {
      cat("\n>>> WARNING: Sphericity VIOLATED (Mauchly p < 0.05). Use Greenhouse-Geisser correction.\n")
    } else {
      cat("\n>>> OUTCOME: Sphericity RESPECTED (Mauchly p >= 0.05).\n")
    }

  } else {
    # Normality VIOLATED --> FRIEDMAN
    friedman <- friedman.test(value ~ time | id, data = df_sub)
    df_friedman <- data.frame(
      chi_squared = friedman$statistic,
      dof = friedman$parameter,
      p_value = friedman$p.value 
    )
    row.names(df_friedman) <- NULL
    table_path <- file.path("export", paste0("df_friedman_", v, ".rds"))
    saveRDS(df_friedman, file = table_path)
    
    # Post-hoc compariosn --> DURBIN/CONOVER pairwise
    conover <- frdAllPairsConoverTest(
      y = df_sub$value,
      groups = df_sub$time,
      blocks = df_sub$id,
      p.adjust.method = "holm"
    )
    stat_mat <- conover$statistic
    p_mat <- conover$p.value
    extract_conover_table <- function(stat_m, p_m) {
      df_stat <- as.data.frame(as.table(stat_m)) |>
        rename(Group1 = Var1, Group2 = Var2, Statistic = Freq) |>
        drop_na()
      df_p <- as.data.frame(as.table(p_m)) |>
        rename(Group1 = Var1, Group2 = Var2, p_value = Freq) |>
        drop_na()
      df_final <- df_stat |>
        left_join(df_p, by = c("Group1", "Group2")) |>
        mutate(
          p_format = sprintf("%.3f", p_value),
          Statistic = sprintf("%.2f", round(Statistic, 2))
        ) |>
        select(Group1, Group2, Statistic, p_format)
      return(df_final)
    }
    df_conover <- extract_conover_table(stat_mat, p_mat)
    table_path <- file.path("export", paste0("df_conover_", v, ".rds"))
    saveRDS(df_conover, file = table_path)
    
    # # 5. Creation and saving of the plot with post-hoc significance bars
    # df_p_values <- as.data.frame(as.table(p_mat)) |>
    #   rename(group1 = Var1, group2 = Var2, p = Freq) |>
    #   drop_na() |>
    #   mutate(
    #     p.label = case_when(
    #       p < 0.001 ~ "p < 0.001",
    #       TRUE ~ paste0("p = ", sprintf("%.3f", p))
    #     )
    #   )
    # 
    # if (nrow(df_p_values) > 0) {
    #   max_val <- max(df_sub$value, na.rm = TRUE)
    #   df_p_values <- df_p_values |>
    #     mutate(y.position = seq(max_val * 1.05, max_val * 1.45, length.out = n()))
    # 
    #   p_grafico <- ggplot(df_sub, aes(x = time, y = value)) +
    #     geom_line(aes(group = id), color = "gray70", alpha = 0.5, linewidth = 0.6) +
    #     geom_point(aes(color = time), alpha = 0.7, size = 2) +
    #     geom_boxplot(aes(fill = time), alpha = 0.4, outlier.shape = NA, width = 0.4) +
    #     stat_pvalue_manual(
    #       df_p_values,
    #       label = "p.label",
    #       y.position = "y.position",
    #       tip.length = 0.01,
    #       size = 3.5
    #     ) +
    #     scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    #     labs(
    #       title = paste("Temporal trend - Variable:", toupper(v)),
    #       subtitle = "Non-parametric comparison (Friedman Test + Conover)",
    #       x = "Control timepoint",
    #       y = "Value (Score)"
    #     ) +
    #     theme_classic(base_size = 12) +
    #     theme(
    #       legend.position = "none",
    #       plot.title = element_text(face = "bold", hjust = 0.5),
    #       plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    #       axis.text.x = element_text(margin = margin(t = 5))
    #     )
    # 
    #   print(p_grafico)


      # Automatically save the plot in the export folder
      # ggsave(
      #   filename = paste0("export/trend_plot_", v, ".png"),
      #   plot = p_grafico,
      #   width = 7,
      #   height = 5,
      #   dpi = 300
      # )
    }
  }
  
  
  
  # Filtra dall'environment tutti gli oggetti che iniziano con "df_"
  list_tabs <- ls(envir = .GlobalEnv)[grepl("^df_", ls(envir = .GlobalEnv))]

  # Filtra dall'environment tutti gli oggetti che iniziano con "plot_" oppure "plot_qq_"
  list_plots <- ls(envir = .GlobalEnv)[grepl("^plot_|^plot_qq_", ls(envir = .GlobalEnv))]

  # Export tables as RDS files
  for (table_name in list_tabs) {
    current_table <- get(table_name)
    table_path <- file.path("export", paste0(table_name, ".rds"))
    saveRDS(current_table, file = table_path)
  }

  # Export tables as XLSX file (multiple sheets)
  write.xlsx(
    setNames(lapply(list_tabs, get), list_tabs),
    file = "export/FINAL_ANALYSIS.xlsx"
  )

  # # Export ggplot graphs as SVG and PDF
  # for (plot_name in list_plots) {
  #   current_plot <- get(plot_name)

  #   # Export as SVG
  #   plot_path_svg <- file.path("export", paste0(plot_name, ".svg"))
  #   ggsave(
  #     filename = plot_path_svg,
  #     plot = current_plot,
  #     width = 8,
  #     height = 6,
  #     units = "in"
  #   )
  # 
  #   # Export as PDF
  #   plot_path_pdf <- file.path("export", paste0(plot_name, ".pdf"))
  #   ggsave(
  #     filename = plot_path_pdf,
  #     plot = current_plot,
  #     width = 8,
  #     height = 6,
  #     units = "in"
  #   )
  #   
  #   # Export as PNG
  #   plot_path_png <- file.path("export", paste0(plot_name, ".png"))
  #   ggsave(
  #     filename = plot_path_png,
  #     plot = current_plot,
  #     width = 8,
  #     height = 6,
  #     units = "in"
  #   )
  # }