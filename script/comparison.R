# Select complete cases for each variable
analyzed_vars <- unique(df_long$var)

# Perform comparative analysis
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
  
  # Check for normality
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
      Statistic = ranova$ANOVA$F,
      DoF       = ranova$ANOVA$DFn,
      p         = ranova$ANOVA$p,
      p_mauchly = p_mauchly,
      GGe       = gge
    )
    table_path <- file.path("export/rds", paste0("df_anova_", v, ".rds"))
    saveRDS(df_anova, file = table_path)
    # Check for sphericity (Mauchly's p)
    if (p_mauchly >= 0.05) {
      # Sphericity RESPECTED --> ANOVA
      df_anova_corrected <- data.frame(
        Statistic = ranova$ANOVA$F,
        DoF       = as.character(ranova$ANOVA$DFn),
        'P-value' = ranova$ANOVA$p,
        Correction = "None",
        check.names = FALSE
      )
      table_path <- file.path("export/rds", paste0("df_anova_corrected_", v, ".rds"))
      saveRDS(df_anova_corrected, file = table_path)
    } else {
      # Sphericity violated --> Use correction
      # (GG vs HF depending on GGe)
      if (gge < 0.75) {
        # Use Greenhouse-Geisser correction
        df_anova_corrected <- data.frame(
          Statistic = ranova$ANOVA$F,
          DoF       = ranova$`Sphericity Corrections`$`DF[GG]`,
          'P-value' = ranova$`Sphericity Corrections`$`p[GG]`,
          Correction = "Greenhouse - Geisser",
          check.names = FALSE
        )
        table_path <- file.path("export/rds", paste0("df_anova_corrected_", v, ".rds"))
        saveRDS(df_anova_corrected, file = table_path)
      } else {
        # Use Huynh-Feldt correction
        df_anova_corrected <- data.frame(
          Statistic = ranova$ANOVA$F,
          DoF       = ranova$`Sphericity Corrections`$`DF[HF]`,
          'P-value' = ranova$`Sphericity Corrections`$`p[HF]`,
          Correction = "Huynh - Feldt",
          check.names = FALSE
        )
        table_path <- file.path("export/rds", paste0("df_anova_corrected_", v, ".rds"))
        saveRDS(df_anova_corrected, file = table_path)
      }
    }
    # Post-hoc comparison --> HOLM/BONFERRONI pairwise (with 95% CI and Cohen's d)
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
    source("script/plot_anova.R")
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
    # Post-hoc comparison --> DURBIN/CONOVER pairwise (with 95% CI on median diff)
    conover <- frdAllPairsConoverTest(
      y = df_sub$value,
      groups = df_sub$time,
      blocks = df_sub$id,
      p.adjust.method = "holm"
    )
    stat_mat <- conover$statistic
    p_mat <- conover$p.value
    extract_conover_table <- function(stat_m, p_m, data) {
      df_stat <- as.data.frame(as.table(stat_m)) |>
        rename(Group2 = Var1, Group1 = Var2, Statistic = Freq) |>
        drop_na()
      df_p <- as.data.frame(as.table(p_m)) |>
        rename(Group2 = Var1, Group1 = Var2, p_value = Freq) |>
        drop_na()
      df_final <- df_stat |>
        left_join(df_p, by = c("Group1", "Group2"))
      
      ci_res <- lapply(seq_len(nrow(df_final)), function(i) {
        g1 <- as.character(df_final$Group1[i])
        g2 <- as.character(df_final$Group2[i])
        v1 <- data$value[data$time == g1]
        v2 <- data$value[data$time == g2]
        wt <- suppressWarnings(wilcox.test(v1, v2, paired = TRUE, conf.int = TRUE))
        data.frame(
          med_diff  = as.numeric(wt$estimate),
          ci_lower  = wt$conf.int[1],
          ci_upper  = wt$conf.int[2]
        )
      })
      df_ci <- do.call(rbind, ci_res)
      df_final <- df_final |>
        bind_cols(df_ci) |>
        select(Group1, Group2, med_diff, ci_lower, ci_upper, Statistic, p_value)
      return(df_final)
    }
    df_conover <- extract_conover_table(stat_mat, p_mat, df_sub)
    table_path <- file.path("export/rds", paste0("df_conover_", v, ".rds"))
    saveRDS(df_conover, file = table_path)
    # Post-hoc comparison --> DURBIN/CONOVER pairwise
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
    source("script/plot_friedman.R")
  }
}