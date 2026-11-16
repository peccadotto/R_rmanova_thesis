tables2export <- list.files(path = "export/rds", pattern = "^df.*\\.rds$", full.names = TRUE)
sheets <- sub("\\.rds$", "", basename(tables2export))
write.xlsx(
  setNames(lapply(tables2export, readRDS), sheets),
  file = "export/Final_analysis.xlsx"
)
message("RDS and XLS export completed")