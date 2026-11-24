# Repeated Measures Statistical Analysis Pipeline

Automated **R** and **Quarto** workflow developed for thesis statistical analysis of longitudinal repeated-measures clinical data. It covers sample size and power calculation, descriptive statistics, distribution and Q-Q plots, automated parametric/non-parametric repeated-measures comparisons, multi-sheet Excel export, and a formatted LaTeX PDF report.

------------------------------------------------------------------------

## Project Structure

``` text
.
├── main.R                     # Master pipeline script (steps 1–8)
├── report.qmd                 # Quarto source for the final PDF report
├── report.pdf                 # Compiled PDF report
├── data/
│   └── data.xlsx              # Input dataset (wide format: id + <var>_<time>)
├── script/
│   ├── functions.R            # Reusable stats, Conover tidying, and plotting helpers
│   ├── setup.R                # Package loading, export folder reset, ggplot2 theme
│   ├── import.R               # Excel import, cleaning, wide-to-long reshaping
│   ├── sample.R               # Paired t-test sample size table (alpha = 0.05)
│   ├── power.R                # Post-hoc power calculation for observed N
│   ├── descr.R                # Descriptive statistics per variable/timepoint
│   ├── plot_descr.R           # Density histograms (mean/median) and Q-Q plots
│   ├── comparison.R           # Repeated-measures ANOVA vs. Friedman + post-hoc
│   └── export.R               # Multi-sheet .xlsx export from RDS tables
└── export/
    ├── final_analysis.xlsx    # Combined Excel workbook of all summary tables
    ├── img/                   # Generated PDF figures (descriptive, Q-Q, trend)
    └── rds/                   # Intermediate serialized R tables (.rds)
```

------------------------------------------------------------------------

## Statistical Workflow

1.  **Data Import (`script/import.R`)**
    - Reads the single Excel workbook in `data/` (`skip = 2`), standardizes column names with `janitor::clean_names()`, and reshapes `<var>_<time>` columns into long format with numerically ordered timepoints.
2.  **Sample Size & Power (`script/sample.R`, `script/power.R`)**
    - Computes required sample sizes (power = `0.80`, `0.90`, `0.95`, `0.99`) and achieved power for the observed cohort `N` across effect sizes (`Cohen's d = 0.3, 0.4, 0.5`) at `alpha = 0.05` using two-sided paired *t*-tests.
3.  **Descriptive Analysis (`script/descr.R`, `script/plot_descr.R`)**
    - Calculates valid/missing counts, mean, SD, 95% *t*-confidence intervals, min, quartiles (`Q1`, `Q2`, `Q3`), max, and Shapiro-Wilk normality tests.
    - Exports density-scaled histograms with KDE curves (mean/median markers) and normal Q-Q plots.
4.  **Longitudinal Comparisons (`script/comparison.R`)**
    - Restricts each variable to **complete cases** across all timepoints and checks normality per timepoint via Shapiro-Wilk (`p > 0.05`):
      - **Normality respected:** Repeated-measures ANOVA (`rstatix::anova_test`), Mauchly's sphericity test with automatic Greenhouse-Geisser (`gge < 0.75`) or Huynh-Feldt (`gge >= 0.75`) correction, Holm-adjusted paired *t*-tests, and paired Cohen's *d*.
      - **Normality violated:** Friedman test (`friedman.test`), Holm-adjusted Conover all-pairs post-hoc tests (`PMCMRplus::frdAllPairsConoverTest`), and paired Wilcoxon Hodges-Lehmann pseudomedian differences with 95% CIs.
    - Generates trend boxplots with stacked significance brackets via `func_plot_trend()`.
5.  **Export & Cleanup (`script/export.R`, `main.R`)**
    - Bundles all `export/rds/df*.rds` tables into `export/final_analysis.xlsx`.
    - Cleans the R global environment at the end of `main.R`, retaining only `df_*`, `plot_*`, and `my_plot_theme`.

------------------------------------------------------------------------

## Data & Privacy Notice

- The dataset stored in `data/data.xlsx` contains **real clinical data** that were **fully anonymized prior to use** (subject identifiers were replaced with pseudonymous codes, e.g., `PZ01L`).
- Input columns follow the naming convention `<variable>_<timepoint>` (e.g., `ai_0`, `ai_1`, `ai_12`, `ai_24`).

------------------------------------------------------------------------

## Requirements & Usage

### Prerequisites

- **R** (developed with R `4.6.1`)
- **Quarto** + a LaTeX engine (`pdflatex`) to compile `report.qmd`
- CRAN packages (automatically checked and installed by `script/setup.R`): `geomtextpath`, `ggpubr`, `janitor`, `openxlsx`, `PMCMRplus`, `readxl`, `rstatix`, `tidyverse`

### Run the Analysis & Build the Report

1.  Run the full R pipeline from the project root:

    ``` r
    source("main.R")
    ```

2.  Render the Quarto PDF report:

    ``` bash
    quarto render report.qmd
    ```

The final outputs will be available in `export/final_analysis.xlsx`, `export/img/`, `export/rds/`, and `report.pdf`.
