# if (!dir.exists("export")) {
#   dir.create("export")
#   message("Folder 'export' created")
# }


# ==============================================================================
# REPEATED MEASURES ANOVA
# ==============================================================================

# 1. CHECK AND INSTALL REQUIRED PACKAGES ---------------------------------------

source("packages.R")

# 2. DATABASE IMPORT -----------------------------------------------------------

source("import.R")

# 3. SAMPLE SIZE CALCULATION ---------------------------------------------------
# 4. POWER CALCULATION ---------------------------------------------------------

source("sample_power.R")

# 5. START OF REPEATED MEASURES STATISTICAL ANALYSIS ---------------------------

analyzed_vars <- unique(df_long$var)

for (v in analyzed_vars) {
  
  cat("\n===================================================================\n")
  cat(sprintf(
    " %s. ANALYSIS IN PROGRESS FOR [%s]\n",
    match(v, analyzed_vars),
    toupper(v)
    )
  )
  cat("=====================================================================\n")
  
  df_sub <- df_long |> filter(var == v) |> drop_na(value)
  
  # 2. Handle potential duplicates (if multiple rows exist for the same id and time, take the mean)
  df_sub <- df_sub %>% 
    group_by(id, time) %>% 
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  unique_timepoints <- sort(unique(df_sub$time))
  n_timepoints <- length(unique_timepoints)
  
  # 4. Select ONLY patients who have EXACTLY one measurement for EVERY expected timepoint
  valid_ids <- df_sub %>%
    group_by(id) %>%
    summarise(
      n_osservazioni = n(), 
      ha_tutti_i_tempi = all(unique_timepoints %in% time), 
      .groups = "drop"
    ) %>%
    filter(n_osservazioni == n_timepoints & ha_tutti_i_tempi) %>%
    pull(id)
  
  df_sub <- df_sub %>% 
    filter(id %in% valid_ids) %>%
    mutate(
      id = factor(id),
      time = factor(time, levels = unique_timepoints)
    )
  
  cat(sprintf(">>> Subjects included in the balanced analysis: %d\n", length(unique(df_sub$id))))
  
  # --- FROM HERE ON CONTINUES THE REST OF THE SCRIPT (Shapiro, Friedman, etc.) ---
  
  # 2. Normality analysis for the current variable
  df_shapiro <- df_sub %>%
    group_by(time) %>%
    shapiro_test(value) %>%
    data.frame() %>%
    select(-any_of("var")) %>%
    mutate(
      normal = ifelse(p > 0.05, "Yes (p > 0.05)", "No (p < 0.05)")
    )
  print(df_shapiro)
  
  # Generate QQ-plot and save it (optional, useful for visual inspection)
  p_qq <- ggqqplot(df_sub, x = "value", facet.by = "time") +
    ggtitle(paste("QQ-Plot -", v))
  print(p_qq)
  
  # 3. Sphericity / homogeneity check (Levene)
  levene_risultato <- leveneTest(value ~ time, df_sub)
  print(levene_risultato)
  
  # 4. Parametric (ANOVA) vs Non-Parametric (Friedman) Branching
  if (all(df_shapiro$p > 0.05)) {
    cat("\n--------------------------------------------------------\n")
    cat(sprintf(">>> [%s] OUTCOME: Normality RESPECTED (all p > 0.05).\n", v))
    cat(">>> Running Repeated Measures ANOVA...\n")
    cat("--------------------------------------------------------\n")
    
    anova_ripetuta <- df_sub %>%
      anova_test(dv = value, wid = id, within = time)
    print(anova_ripetuta)
    
    p_mauchly <- anova_ripetuta$`p.Mauchly`
    
    if (!is.null(p_mauchly) && p_mauchly < 0.05) {
      cat("\n>>> WARNING: Sphericity VIOLATED (Mauchly p < 0.05). Use Greenhouse-Geisser correction.\n")
    } else {
      cat("\n>>> OUTCOME: Sphericity RESPECTED (Mauchly p >= 0.05).\n")
    }
    
  } else {
    cat("\n--------------------------------------------------------\n")
    cat(sprintf(">>> [%s] OUTCOME: Normality VIOLATED (at least one p < 0.05).\n", v))
    cat(">>> Running FRIEDMAN TEST + Conover...\n")
    cat("--------------------------------------------------------\n")
    
    friedman <- friedman.test(value ~ time | id, data = df_sub)
    print(friedman)
    
    conover <- frdAllPairsConoverTest(
      y = df_sub$value,
      groups = df_sub$time,
      blocks = df_sub$id,
      p.adjust.method = "holm"
    )
    
    stat_mat <- conover$statistic
    p_mat <- conover$p.value
    
    extract_conover_table <- function(stat_m, p_m) {
      df_stat <- as.data.frame(as.table(stat_m)) %>% 
        rename(Group1 = Var1, Group2 = Var2, Statistic = Freq) %>% 
        drop_na()
      
      df_p <- as.data.frame(as.table(p_m)) %>% 
        rename(Group1 = Var1, Group2 = Var2, p_value = Freq) %>% 
        drop_na()
      
      df_final <- df_stat %>%
        left_join(df_p, by = c("Group1", "Group2")) %>%
        mutate(
          p_format = case_when(
            p_value < 0.001 ~ "< .001",
            TRUE ~ sprintf("%.3f", p_value)
          ),
          Statistic = sprintf("%.2f", round(Statistic, 2)),
          Sign = "-"
        ) %>%
        select(Group1, Sign, Group2, Statistic, p_format)
      
      return(df_final)
    }
    
    tabella_conover <- extract_conover_table(stat_mat, p_mat)
    cat(sprintf("\n--- Pairwise Comparisons (Durbin-Conover) for %s ---\n", v))
    print(as_tibble(tabella_conover), n = Inf)
    
    # 5. Creation and saving of the plot with post-hoc significance bars
    df_p_values <- as.data.frame(as.table(p_mat)) %>%
      rename(group1 = Var1, group2 = Var2, p = Freq) %>%
      drop_na() %>%
      mutate(
        p.label = case_when(
          p < 0.001 ~ "p < 0.001",
          TRUE ~ paste0("p = ", sprintf("%.3f", p))
        )
      )
    
    if (nrow(df_p_values) > 0) {
      max_val <- max(df_sub$value, na.rm = TRUE)
      df_p_values <- df_p_values %>%
        mutate(y.position = seq(max_val * 1.05, max_val * 1.45, length.out = n()))
      
      p_grafico <- ggplot(df_sub, aes(x = time, y = value)) +
        geom_line(aes(group = id), color = "gray70", alpha = 0.5, linewidth = 0.6) +
        geom_point(aes(color = time), alpha = 0.7, size = 2) +
        geom_boxplot(aes(fill = time), alpha = 0.4, outlier.shape = NA, width = 0.4) +
        stat_pvalue_manual(
          df_p_values, 
          label = "p.label", 
          y.position = "y.position",
          tip.length = 0.01,
          size = 3.5
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
        labs(
          title = paste("Temporal trend - Variable:", toupper(v)),
          subtitle = "Non-parametric comparison (Friedman Test + Conover)",
          x = "Control timepoint",
          y = "Value (Score)"
        ) +
        theme_classic(base_size = 12) +
        theme(
          legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
          axis.text.x = element_text(margin = margin(t = 5))
        )
      
      print(p_grafico)
      
      # Automatically save the plot in the export folder
      ggsave(
        filename = paste0("export/trend_plot_", v, ".png"), 
        plot = p_grafico, 
        width = 7, 
        height = 5, 
        dpi = 300
      )
    }
  }
}