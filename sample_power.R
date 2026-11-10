# Fixed values
N <- nrow(df)
alpha <- 0.05
delta_vals <- c(0.3, 0.4, 0.5)
power_vals <- c(0.80, 0.90, 0.95, 0.99)

# Calculated values
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
df_sample_size <- data.frame(Power = power_vals, sample_size)
df_powers <- as.data.frame(t(power_values))
colnames(sample_size) <- paste0("δ ", delta_vals)
colnames(df_powers) <- paste0("Delta = ", delta_vals)
print("--- Table 1: Sample size (N) ---")
print(df_sample_size)
print(paste("--- Table 2: Powers with N =", N, "---"))
print(df_powers)