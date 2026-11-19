packages <- c(
  "afex",
  "car",
  "dplyr",
  "emmeans",
  "geomtextpath",
  "ggpubr",
  "janitor",
  "openxlsx",
  "PMCMRplus",
  "purrr",
  "readxl",
  "rstatix",
  "tibble",
  "tidyverse"
)

func_check_packages(packages)

folders2create <- c(
  "export",
  "export/rds",
  "export/img"
)
for (folder in folders2create){
  if (!dir.exists(folder)) {
    dir.create(folder)
    message(paste0("Folder '", folder ,"' created"))
  }
}

my_plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      hjust = 0.5,
      margin = margin(t = 10, b = 20)
    ),
    axis.title = element_text(
      face = "bold",
      size = 16,
      color = "black"
    ),
    axis.text = element_text(
      face = "bold",
      size = 16,
      color = "black"
    ),
    axis.line = element_line(
      color = "black",
      linewidth = 0.5
    ),
    axis.ticks = element_line(
      color = "black"
    )
  )