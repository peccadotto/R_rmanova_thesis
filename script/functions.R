# Check for missing CRAN packages, install them if needed, and attach all packages
func_check_packages <- function(pkg_list) {
  new_packages <- pkg_list[!(pkg_list %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    message(
      "The following missing packages will be installed: ",
      paste(new_packages, collapse = ", ")
    )
    install.packages(new_packages)
  } else {
    message("All required packages are already installed")
  }
  invisible(lapply(pkg_list, library, character.only = TRUE))
  message("All packages have been successfully loaded")
}

# Compute parametric (mean, SD, 95% t-CI) and non-parametric (min, quartiles, max)
# descriptive statistics plus the Shapiro-Wilk normality test for one column
func_descriptive <- function(df, var_name) {
  x <- df[[var_name]]
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  # Require > 1 valid observation and non-zero SD to compute a finite t-based 95% CI
  if (n_valid > 1 && !is.na(sd_val) && sd_val > 0) {
    se_val <- sd_val / sqrt(n_valid)
    ci_t <- qt(0.975, df = n_valid - 1)
    ci_lower <- mean_val - ci_t * se_val
    ci_upper <- mean_val + ci_t * se_val
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }
  if (n_valid >= 3 && n_valid <= 5000 && !is.na(sd_val) && sd_val > 0) {
    sw <- shapiro.test(x)
    sw_w <- unname(sw$statistic)
    sw_p <- sw$p.value
  } else {
    sw_w <- NA_real_
    sw_p <- NA_real_
  }
  perc <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(
    var_name, n_valid, n_missing,
    mean = mean_val, sd = sd_val, ci_lower, ci_upper,
    min = min_val, q1 = perc[[1]], q2 = perc[[2]], q3 = perc[[3]], max = max_val, sw_w, sw_p
  )
}

# Tidy Conover post-hoc matrices and pair each comparison with the paired Wilcoxon
# Hodges-Lehmann pseudomedian difference and its 95% confidence interval
func_extract_conover_table <- function(stat_m, p_m, data) {
  df_stat <- as.data.frame(as.table(stat_m)) |>
    rename(group2 = Var1, group1 = Var2, statistic = Freq) |>
    drop_na()
  df_p <- as.data.frame(as.table(p_m)) |>
    rename(group2 = Var1, group1 = Var2, p_value = Freq) |>
    drop_na()
  df_final <- df_stat |>
    left_join(df_p, by = c("group1", "group2"))

  ci_res <- lapply(seq_len(nrow(df_final)), function(i) {
    g1 <- as.character(df_final$group1[i])
    g2 <- as.character(df_final$group2[i])
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
    select(group1, group2, med_diff, ci_lower, ci_upper, statistic, p_value)
  return(df_final)
}

func_plot_trend <- function(df_sub, df_p_values, var_name, test_label, caption_text, formats = c("pdf")) {
  if (nrow(df_p_values) == 0) {
    return(invisible(NULL))
  }

  message("Starting to create plots (", test_label, ") for [", toupper(var_name), "]")

  min_val <- min(df_sub$value, na.rm = TRUE)
  max_val <- max(df_sub$value, na.rm = TRUE)
  rng_data <- max_val - min_val
  if (rng_data == 0) rng_data <- ifelse(max_val == 0, 1, abs(max_val) * 0.1)

  n_bars <- nrow(df_p_values)
  pad_bottom <- 0.05
  gap_first <- 0.06
  step_frac <- 0.085
  pad_top <- 0.10

  used_frac <- pad_bottom + gap_first + (n_bars - 1) * step_frac + pad_top
  total_y_span <- rng_data / (1 - used_frac)

  y_min <- min_val - pad_bottom * total_y_span
  y_max <- max_val + (gap_first + (n_bars - 1) * step_frac + pad_top) * total_y_span
  step_units <- step_frac * total_y_span

  df_p_values <- df_p_values |>
    mutate(y.position = (max_val + gap_first * total_y_span) + (row_number() - 1) * step_units)

  p_plot <- ggplot(df_sub, aes(x = time, y = value)) +
    geom_boxplot(aes(fill = time), alpha = 0.4, outlier.shape = NA, width = 0.4, show.legend = FALSE) +
    stat_pvalue_manual(
      df_p_values,
      label = "p.label",
      y.position = "y.position",
      tip.length = 0.01,
      bracket.size = 0.8,
      size = 3.5
    ) +
    scale_y_continuous(
      limits = c(y_min, y_max),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      title = toupper(var_name),
      caption = caption_text,
      x = NULL,
      y = NULL
    ) +
    my_plot_theme

  if (interactive()) print(p_plot)
  for (ext in formats) {
    ggsave(
      filename = paste0("export/img/plot_trend_", var_name, ".", ext),
      plot = p_plot,
      width = 8,
      height = 6,
      units = "in",
      dpi = 300
    )
  }
  message("✔ Plot for [", toupper(var_name), "] created")
  invisible(p_plot)
}
