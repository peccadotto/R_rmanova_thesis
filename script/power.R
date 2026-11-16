power_values <- sapply(delta_vals, function(d) {
  res <- power.t.test(
    n = N,
    d = d,
    sig.level = alpha,
    type = "paired",
    alternative = "two.sided"
  )
  round(res$power, 3)
})

df_powers <- as_tibble(t(power_values))
colnames(df_powers) <- paste0("Delta ", delta_vals)
table_path <- file.path("export/rds/df_powers.rds")
saveRDS(df_powers, file = table_path)
rm(N, alpha, delta_vals, power_values, table_path)