# Collect all exported RDS summary tables and write each one as a sheet in Final_analysis.xlsx
tables_to_export <- list.files(
  path = "export/rds",
  pattern = "^df.*\\.rds$",
  full.names = TRUE
)
sheets <- sub("\\.rds$", "", basename(tables_to_export))
write.xlsx(
  setNames(lapply(tables_to_export, readRDS), sheets),
  file = "export/final_analysis.xlsx"
)
message("RDS and XLSX export completed")
