excel_files <- list.files(
  path = "data", 
  pattern = "(?i)\\.xlsx?$", 
  full.names = TRUE
)
excel_files <- excel_files[!grepl("^~\\$", basename(excel_files))]

df <- read_excel(excel_files, skip = 2) |> clean_names()

df_long <- df |>
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