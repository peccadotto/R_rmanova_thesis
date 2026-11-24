# Select complete cases for each variable (required by repeated-measures ANOVA and Friedman)
analyzed_vars <- unique(df_long$var)

# Perform comparative analysis
for (v in analyzed_vars) {
  df_sub <- df_long |>
    filter(var == v) |>
    drop_na(value)
  # Collapse duplicate id-time entries by averaging before checking completeness
  df_sub <- df_sub |>
    group_by(id, time) |>
    summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
  unique_timepoints <- sort(unique(df_sub$time))
  n_timepoints <- length(unique_timepoints)
  # Retain only subjects measured at every timepoint
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

  # Check for normality
  df_shapiro <- df_sub |>
    group_by(time) |>
    shapiro_test(value) |>
    data.frame() |>
    select(-any_of("var")) |>
    mutate(
      normal = ifelse(p > 0.05, "Yes (p > 0.05)", "No (p < 0.05)")
    ) |>
    left_join(
      df_sub |>
        group_by(time) |>
        summarise(
          mean = mean(value, na.rm = TRUE),
          median = median(value, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "time"
    )
  if (all(df_shapiro$p > 0.05)) {
    is_normal <- TRUE
  } else {
    is_normal <- FALSE
  }
  table_path <- file.path("export/rds", paste0("df_shapiro_", v, ".rds"))
  saveRDS(df_shapiro, file = table_path)

  # Select the analysis to be performed
  # (ANOVA vs Friedman | Normality respected vs violated)

  if (is_normal) {
    # Normality RESPECTED --> ANOVA
    ranova <- df_sub |>
      anova_test(dv = value, wid = id, within = time)
    p_mauchly <- ranova$`Mauchly's Test for Sphericity`$p
    gge <- ranova$`Sphericity Corrections`$GGe
    df_anova <- data.frame(
      statistic = ranova$ANOVA$F,
      dof       = ranova$ANOVA$DFn,
      p         = ranova$ANOVA$p,
      p_mauchly = p_mauchly,
      gge       = gge
    )
    table_path <- file.path("export/rds", paste0("df_anova_", v, ".rds"))
    saveRDS(df_anova, file = table_path)
    # Check for sphericity (Mauchly's p)
    if (p_mauchly >= 0.05) {
      # Sphericity RESPECTED --> ANOVA
      df_anova_corrected <- data.frame(
        statistic = ranova$ANOVA$F,
        dof = as.character(ranova$ANOVA$DFn),
        p_value = ranova$ANOVA$p,
        correction = "None",
        check.names = FALSE
      )
      table_path <- file.path("export/rds", paste0("df_anova_corrected_", v, ".rds"))
      saveRDS(df_anova_corrected, file = table_path)
    } else {
      # Sphericity violated --> Use correction
      # (Girden's rule: GG if epsilon < 0.75, otherwise HF)
      if (gge < 0.75) {
        # Use Greenhouse-Geisser correction
        df_anova_corrected <- data.frame(
          statistic = ranova$ANOVA$F,
          dof = ranova$`Sphericity Corrections`$`DF[GG]`,
          p_value = ranova$`Sphericity Corrections`$`p[GG]`,
          correction = "Greenhouse - Geisser",
          check.names = FALSE
        )
        table_path <- file.path("export/rds", paste0("df_anova_corrected_", v, ".rds"))
        saveRDS(df_anova_corrected, file = table_path)
      } else {
        # Use Huynh-Feldt correction
        df_anova_corrected <- data.frame(
          statistic = ranova$ANOVA$F,
          dof = ranova$`Sphericity Corrections`$`DF[HF]`,
          p_value = ranova$`Sphericity Corrections`$`p[HF]`,
          correction = "Huynh - Feldt",
          check.names = FALSE
        )
        table_path <- file.path("export/rds", paste0("df_anova_corrected_", v, ".rds"))
        saveRDS(df_anova_corrected, file = table_path)
      }
    }
    # Post-hoc comparison --> HOLM pairwise (with 95% CI and Cohen's d)
    df_p_values <- df_sub |>
      pairwise_t_test(
        value ~ time,
        paired = TRUE,
        p.adjust.method = "holm",
        detailed = TRUE
      )
    df_effsize <- df_sub |>
      cohens_d(
        value ~ time,
        paired = TRUE
      ) |>
      select(group1, group2, effsize, magnitude)

    df_p_values <- df_p_values |>
      left_join(df_effsize, by = c("group1", "group2")) |>
      mutate(
        p.label = case_when(
          p.adj < 0.001 ~ "p < 0.001",
          TRUE ~ paste0("p = ", sprintf("%.3f", p.adj))
        )
      )
    table_path <- file.path("export/rds", paste0("df_p_values_", v, ".rds"))
    saveRDS(df_p_values, file = table_path)
    # Comparison plots
    func_plot_trend(
      df_sub, df_p_values, v,
      test_label = "ANOVA",
      caption_text = "Parametric comparison (Repeated measures ANOVA + Holm pairwise comparison)",
      formats = formats
    )
  } else {
    # Normality VIOLATED --> FRIEDMAN
    friedman <- friedman.test(value ~ time | id, data = df_sub)
    df_friedman <- data.frame(
      chi_squared = friedman$statistic,
      dof = friedman$parameter,
      p_value = friedman$p.value
    )
    row.names(df_friedman) <- NULL
    table_path <- file.path("export/rds", paste0("df_friedman_", v, ".rds"))
    saveRDS(df_friedman, file = table_path)
    # Post-hoc comparison --> CONOVER pairwise (with 95% CI on Hodges-Lehmann pseudomedian diff)
    conover <- frdAllPairsConoverTest(
      y = df_sub$value,
      groups = df_sub$time,
      blocks = df_sub$id,
      p.adjust.method = "holm"
    )
    stat_mat <- conover$statistic
    p_mat <- conover$p.value
    df_conover <- func_extract_conover_table(stat_mat, p_mat, df_sub)
    table_path <- file.path("export/rds", paste0("df_conover_", v, ".rds"))
    saveRDS(df_conover, file = table_path)
    # Post-hoc comparison --> CONOVER pairwise
    df_p_values <- as.data.frame(as.table(p_mat)) |>
      rename(group1 = Var1, group2 = Var2, p = Freq) |>
      drop_na() |>
      mutate(
        p.label = case_when(
          p < 0.001 ~ "p < 0.001",
          TRUE ~ paste0("p = ", sprintf("%.3f", p))
        )
      )
    table_path <- file.path("export/rds", paste0("df_p_values_", v, ".rds"))
    saveRDS(df_p_values, file = table_path)
    # Comparison plots
    func_plot_trend(
      df_sub, df_p_values, v,
      test_label = "Friedman's test",
      caption_text = "Non-parametric comparison (Friedman's test + Conover pairwise comparison)",
      formats = formats
    )
  }
}
