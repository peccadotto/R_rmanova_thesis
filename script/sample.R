alpha <- 0.05
delta_vals <- c(0.3, 0.4, 0.5)
power_vals <- c(0.80, 0.90, 0.95, 0.99)

sample_size <- sapply(delta_vals, function(d) {
  sapply(power_vals, function(p) {
    res <- power.t.test(
      d = d,
      sig.level = alpha,
      power = p,
      type = "paired",
      alternative = "two.sided"
    )
    ceiling(res$n)
  })
})

colnames(sample_size) <- paste0("Delta ", delta_vals)
df_samples <- data.frame(Power = power_vals, sample_size)
table_path <- file.path("export/rds/df_samples.rds")
saveRDS(df_samples, file = table_path)