# 1. PROJECT SETUP -------------------------------------------------------------
source("script/functions.R")
source("script/setup.R")

# 2. DATA IMPORT ---------------------------------------------------------------
source("script/import.R")

# 3. SAMPLE SIZE CALCULATION ---------------------------------------------------
source("script/sample.R")

# 4. POWER CALCULATION ---------------------------------------------------------
source("script/power.R")

# 5. DESCRIPTIVE ANALYSIS ------------------------------------------------------
source("script/descr.R")

# 6. REPEATED MEASURES STATISTICAL ANALYSIS ------------------------------------
source("script/comparison.R")

# 7. DATA EXPORT ---------------------------------------------------------------
source("script/export.R")

# 8. ENVIRONMENT CLEANUP -------------------------------------------------------
# Keep only result data frames (df_*), plots (plot_*), and the shared theme
rm(list = grep("^(df_|plot_|my_plot_theme$)", ls(), value = TRUE, invert = TRUE))

# 9. CREATE REPORT
Sys.setenv(QUARTO_PATH = "C:\\Program Files\\RStudio\\resources\\app\\bin\\quarto\\bin\\quarto.exe")
quarto::quarto_render("report.qmd")
utils::browseURL("report.pdf")
