df_descr <- map_dfr(
  colnames(df_complete[-1]),
  ~ func_descriptive(df_complete, .x)
)
rownames(df_descr) <- NULL
table_path <- file.path("export/rds/df_descr.rds")
saveRDS(df_descr, file = table_path)
source("script/plot_descr.R")