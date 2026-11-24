for (variable in colnames(df_complete)[-1]) {
  # Distribution plot: density-scaled histogram, KDE curve, and mean/median markers
  message("Starting to create plot (descriptive analysis) for [", toupper(variable), "]")
  plot <- ggplot(df_complete, aes(x = .data[[variable]])) +
    geom_histogram(
      aes(y = after_stat(density)),
      bins = 30,
      fill = "white",
      color = "black",
      linewidth = 1
    ) +
    geom_density(
      color = "black",
      linewidth = 1.25
    ) +
    geom_labelvline(
      xintercept = mean(df_complete[[variable]], na.rm = TRUE),
      color = "red",
      linewidth = 1.25,
      label = "MEAN",
      hjust = 0.1,
      size = 16 / ggplot2:::.pt,
      fontface = "bold"
    ) +
    geom_labelvline(
      xintercept = median(df_complete[[variable]], na.rm = TRUE),
      color = "blue",
      linewidth = 1.25,
      label = "MEDIAN",
      hjust = 0.25,
      size = 16 / ggplot2:::.pt,
      fontface = "bold"
    ) +
    labs(
      title = toupper(variable),
      x = NULL,
      y = NULL
    ) +
    coord_cartesian(ylim = c(0, NA), expand = FALSE) +
    my_plot_theme
  obj_name <- paste0("plot_descr_", variable)
  assign(obj_name, plot, envir = .GlobalEnv)
  if (interactive()) print(plot)
  for (ext in formats) {
    plot_path <- file.path("export/img", paste0(obj_name, ".", ext))
    ggsave(
      filename = plot_path,
      plot = plot,
      width = 8,
      height = 6,
      units = "in"
    )
  }
  message(paste0("✔ Plot for [", toupper(variable), "] created"))
  rm(plot)

  # Normal Q-Q plot to visually inspect deviations from Gaussianity
  message("Starting to create QQ-plot for [", toupper(variable), "]")
  plot_qq <- ggplot(df_complete, aes(sample = .data[[variable]])) +
    stat_qq(fill = "white", color = "black", size = 4) +
    stat_qq_line(color = "red", linewidth = 1) +
    labs(
      title = toupper(variable),
      x = NULL,
      y = NULL
    ) +
    my_plot_theme
  obj_name <- paste0("plot_qq_", variable)
  assign(obj_name, plot_qq, envir = .GlobalEnv)
  if (interactive()) print(plot_qq)
  for (ext in formats) {
    plot_path <- file.path("export/img", paste0(obj_name, ".", ext))
    ggsave(
      filename = plot_path,
      plot = plot_qq,
      width = 8,
      height = 6,
      units = "in",
      dpi = 300
    )
  }
  message(paste0("✔ QQ-Plot for [", toupper(variable), "] created"))
  rm(plot_qq, plot_path)
}

message("All the plots have been created")
