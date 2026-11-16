if (nrow(df_p_values) > 0) {
  message(paste0("Starting to create plots (ANOVA) for [", toupper(v) , "]"))
  max_val <- max(df_sub$value, na.rm = TRUE)
  df_p_values <- df_p_values |>
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
      title = paste0("Temporal trend [", toupper(v), "] (Repeated Measures ANOVA + Paired t-test)"),
      x = NULL,
      y = NULL
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      axis.text.x = element_text(margin = margin(t = 5))
    )
  if (interactive()) print(p_grafico)
  for (ext in formats) {
    ggsave(
      filename = paste0("export/img/plot_trend_", v, ".", ext),
      plot = p_grafico,
      width = 7,
      height = 5,
      dpi = 300
    )
  }
  message(paste0("✔ Plot for [", toupper(v) , "] created"))
}