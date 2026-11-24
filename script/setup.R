# Required CRAN packages for data import, statistical testing, plotting, and export
packages <- c(
  "geomtextpath",
  "ggpubr",
  "janitor",
  "openxlsx",
  "PMCMRplus",
  "readxl",
  "rstatix",
  "tidyverse"
)

func_check_packages(packages)

# Ensure output directories exist before running analysis scripts
folders_to_create <- c(
  "export",
  "export/rds",
  "export/img"
)
folders_to_clean <- c("export/rds", "export/img")
for (folder in folders_to_create) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
    message(paste0("Folder '", folder, "' created"))
  } else if (folder %in% folders_to_clean) {
    old_files <- list.files(folder, full.names = TRUE)
    if (length(old_files) > 0) {
      unlink(old_files, recursive = TRUE)
      message(paste0("Folder '", folder, "' cleared"))
    }
  }
}

# Shared ggplot2 theme used across descriptive, QQ, and longitudinal comparison plots
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

# Output image formats shared by descriptive and trend plots
formats <- c("pdf")
