packages <- c(
  "afex",
  "car",
  "emmeans",
  "ggpubr",
  "openxlsx",
  "PMCMRplus",
  "readxl",
  "rstatix",
  "tidyverse"
)

check_packages <- function(pkg_list) {
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

check_packages(packages)

if (!dir.exists("export")) {
  dir.create("export")
  message("Folder 'export' created")
}