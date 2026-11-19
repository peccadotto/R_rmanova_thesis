func_descriptive <- function(df, var_name) {
  x <- df[[var_name]]
  n_valid <- sum(!is.na(x))
  n_missing <- sum(is.na(x))
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  if (n_valid > 1 && !is.na(sd_val) && sd_val > 0) {
    se_val <- sd_val / sqrt(n_valid)
    ci_t <- qt(0.975, df = n_valid - 1)
    ci_lower <- mean_val - ci_t * se_val
    ci_upper <- mean_val + ci_t * se_val
  } else {
    ci_lower <- NA_real_
    ci_upper <- NA_real_
  }
  sw_w <- shapiro.test(x)$statistic
  sw_p <- shapiro.test(x)$p.value
  perc <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  data.frame(
    var_name, n_valid, n_missing,
    mean = mean_val, sd_val, ci_lower, ci_upper,
    min_val, q1 = perc[[1]], q2 = perc[[2]], q3 = perc[[3]], max_val, sw_w, sw_p
  )
}

func_check_packages <- function(pkg_list) {
  new_packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    message(
      "The following missing packages will be installed: ",
      paste(new_packages, collapse = ", ")
    )
    install.packages(new_packages)
  } else {
    message("All required packages are already installed")
  }
  invisible(lapply(pkg_list, library, character.only = TRUE))
  message("All packages have been successfully loaded")
}