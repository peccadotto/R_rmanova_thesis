message("Sto iniziando a creare i grafici...")
for (var in colnames(df)[-1]) {
  plot <- ggplot(df, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "#3182bd", color = "white", alpha = 0.7) +
    geom_density(color = "#de2d26", size = 1) +
    labs(
      title = paste("Distribuzione di", var),
      x = var,
      y = "Densità"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      panel.grid.minor = element_blank()
    )
  obj_name <- paste0("plot_", var)
  assign(obj_name, plot, envir = .GlobalEnv)
  print(plot)
  
  plot_qq <- ggplot(df, aes_string(sample = var)) +
    stat_qq(color = "#3182bd", alpha = 0.7) +
    stat_qq_line(color = "#de2d26", size = 1) +
    labs(
      title = paste("QQ-Plot:", var),
      x = "Quantili Teorici",
      y = "Quantili Campionari"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  obj_name <- paste0("plot_qq_", var)
  assign(obj_name, plot, envir = .GlobalEnv)
  print(plot_qq)
}
message("Tutti i grafici sono stato creati")