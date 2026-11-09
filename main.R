# ==============================================================================
# ANOVA A MISURE RIPETUTE
# ==============================================================================

# --- 1. CHECK AND INSTALL REQUIRED PACKAGES ---

packages <- c(
  "dplyr",
  "ggplot2",
  "ICC",
  "irr",
  "janitor",
  "openxlsx",
  "readxl",
  "svglite",
  "tidyr"
)

check_packages <- function(pkg_list) {
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

check_packages(packages)

# --- 2. CHECK REQUIRED FOLDERS AND FILES ---

if (!dir.exists("data")) {
  dir.create("data")
  message("Folder 'data' created")
}

if (!dir.exists("export")) {
  dir.create("export")
  message("Folder 'export' created")
}

excel_files <- list.files(
  path = "data", 
  pattern = "(?i)\\.xlsx?$", 
  full.names = TRUE
)
excel_files <- excel_files[!grepl("^~\\$", basename(excel_files))]
num_files <- length(excel_files)

if (num_files == 1) {
  my_data <- read_excel(excel_files)
} else if (num_files == 0) {
  stop("Error: No Excel file found in the 'data' folder.")
} else {
  stop(paste(
    "Error: Found", num_files, "Excel files in the 'data' folder.",
    "Please remove unnecessary files."
  ))
}

# --- 3. DATABASE IMPORT AND DATA MANIPULATION ---

df <- read_excel(excel_files) |> clean_names()

# --- 4. CALCOLO DELLA NUMEROSITA CAMPIONARIA ---

alpha <- 0.05
delta_vals <- c(0.3, 0.4, 0.5)
power_vals <- c(0.80, 0.90, 0.95, 0.99)

sample_size_mat <- sapply(delta_vals, function(d) {
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

colnames(sample_size_mat) <- paste0("Delta = ", delta_vals)
df_sample_size <- data.frame(Power = power_vals, sample_size_mat)
print("--- Table 1: Sample size (N) ---")
print(df_sample_size)


# --- 4. CALCOLO DELLA POTENZA ---

n_fixed <- nrow(df)
post_power_values <- sapply(delta_vals, function(d) {
  res <- power.t.test(
    n = n_fixed,
    d = d,
    sig.level = alpha,
    type = "paired",
    alternative = "two.sided"
  )
  round(res$power, 3)
})
df_post_power <- as.data.frame(t(post_power_values))
colnames(df_post_power) <- c("Delta...0.3", "Delta...0.4", "Delta...0.5")
print(paste("--- Table 2: Powers with N =", n_fixed, "---"))
print(df_sample_size)
