formats = c("pdf")

for (var in colnames(df_complete)[-1]) {
  
  message("Starting to create plot (descriptive analysis) for [", toupper(var), "]")
  plot <- ggplot(df_complete, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "white", color = "black", linewidth = 1) +
    geom_density(color = "black", linewidth = 1.25) +
    geom_vline(xintercept = mean(df_complete[[var]], na.rm = TRUE), linetype = "dashed", color = "black", linewidth = 1.25) +
    geom_vline(xintercept = median(df_complete[[var]], na.rm = TRUE), linetype = "solid", color = "black", linewidth = 1.25) +
    labs(
      title = toupper(var),
      caption = "Solid vertical line = Median, Dashed vertical line = Mean",
      x = NULL,
      y = NULL
    ) +
    my_plot_theme
  obj_name <- paste0("plot_descr_", var)
  assign(obj_name, plot, envir = .GlobalEnv)
  if (interactive()) print(plot)
  for (ext in formats){
    plot_path_svg <- file.path("export/img", paste0(obj_name, ".", ext))
    ggsave(
      filename = plot_path_svg,
      plot = plot,
      width = 8,
      height = 6,
      units = "in"
    )
  }
  message(paste0("✔ Plot for [", toupper(var) , "] created"))
  
  message("Starting to create QQ-plot for [", toupper(var), "]")
  plot_qq <- ggplot(df_complete, aes(sample = .data[[var]])) +
    stat_qq(fill = "white", color = "black", size = 4) +
    stat_qq_line(color = "black", linewidth = 1) +
    labs(
      title = toupper(var),
      x = NULL,
      y = NULL
    ) +
    my_plot_theme
  obj_name <- paste0("plot_qq_", var)
  assign(obj_name, plot_qq, envir = .GlobalEnv)
  if (interactive()) print(plot_qq)
  for (ext in formats){
    plot_path_svg <- file.path("export/img", paste0(obj_name, ".", ext))
    ggsave(
      filename = plot_path_svg,
      plot = plot_qq,
      width = 8,
      height = 6,
      units = "in"
    )
  }
  message(paste0("✔ QQ-Plot for [", toupper(var) , "] created"))

}

message("All the plots have been created")