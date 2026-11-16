packages <- c(
  "afex",
  "car",
  "dplyr",
  "emmeans",
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

func_check_packages <- function(pkg_list) {
  new_packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
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

func_check_packages(packages)
rm(func_check_packages)
rm(packages)

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
rm(folders2create)
rm(folder)

my_plot_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, margin = margin(t = 10, b = 20)),
    axis.title = element_text(face = "bold", size = 16, color = "black"),
    axis.text  = element_text(face = "bold", size = 16, color = "black"),
    axis.line  = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black")
  )