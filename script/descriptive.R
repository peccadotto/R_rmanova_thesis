descriptive <- function(df, var_name) {
  x <- df[[var_name]]
  n_valid   <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  mean     <- mean(x, na.rm = TRUE)
  median   <- median(x, na.rm = TRUE)
  sd_val    <- sd(x, na.rm = TRUE)
  min_val   <- min(x, na.rm = TRUE)
  max_val   <- max(x, na.rm = TRUE)
  sw <- shapiro.test(x)
  sw_w <- sw$statistic
  sw_p <- sw$p.value
  perc <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  data.frame(
    var_name, n_valid, n_missing, mean, median, sd_val, min_val, max_val, sw_w, sw_p, perc[1], perc[2], perc[3]
  )
}

df_descr <- map_dfr(colnames(df[-1]), ~ descriptive(df, .x))
rownames(df_descr) <- NULL