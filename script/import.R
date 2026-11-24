# Locate Excel source file in data/ while ignoring temporary Office lock files (~$*)
excel_files <- list.files(
  path = "data",
  pattern = "(?i)\\.xlsx?$",
  full.names = TRUE
)
excel_files <- excel_files[!grepl("^~\\$", basename(excel_files))]

if (length(excel_files) == 0) {
  stop("No Excel file found in 'data/'")
}
if (length(excel_files) > 1) {
  stop("Multiple Excel files found in 'data/': ", paste(basename(excel_files), collapse = ", "))
}

# Skip top 2 metadata rows in the clinical Excel sheet and standardize column names
df_complete <- read_excel(excel_files, skip = 2) |> clean_names()
table_path <- file.path("export/rds/df_complete.rds")
saveRDS(df_complete, file = table_path)

# Reshape wide columns (<var>_<time>) to long format with numerically ordered timepoints
df_long <- df_complete |>
  pivot_longer(
    cols = -1,
    names_to = c("var", "time"),
    names_sep = "_"
  ) |>
  mutate(
    id = as.factor(id),
    time = factor(time, levels = sort(unique(as.numeric(time)))),
    var = as.factor(var)
  )
table_path <- file.path("export/rds/df_long.rds")
saveRDS(df_long, file = table_path)
