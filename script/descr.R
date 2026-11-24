# Compute descriptive statistics for every numeric variable/timepoint (excluding id)
df_descr <- map_dfr(
  colnames(df_complete[-1]),
  ~ func_descriptive(df_complete, .x)
)
rownames(df_descr) <- NULL
table_path <- file.path("export/rds/df_descr.rds")
saveRDS(df_descr, file = table_path)

# Generate histogram/density and QQ-plots for each variable/timepoint
source("script/plot_descr.R")