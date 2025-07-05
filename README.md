# hcruR <img src="man/figures/hcruR.png" align="right" height="130"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/mumbarkar/hcruR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mumbarkar/hcruR/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/mumbarkar/hcruR/graph/badge.svg)](https://app.codecov.io/gh/mumbarkar/hcruR) [![GitHub version](https://img.shields.io/github/v/release/mumbarkar/hcruR)](https://github.com/mumbarkar/hcruR/releases) [![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE) [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

<!-- badges: end -->

**hcruR** is an R package to help health economists and RWE analysts estimate and compare healthcare resource utilization (HCRU) from observational healthcare data, such as claims or electronic health records.

------------------------------------------------------------------------

## 🚀 Features

-   Estimate patient-level HCRU by:
    -   Domain (inpatient, outpatient, pharmacy, etc.)
    -   Time relative to index date (pre/post)
-   Compare HCRU across cohorts
-   Visualize domain-wise HCRU statistics
-   Designed for flexible real-world evidence (RWE) workflows

------------------------------------------------------------------------

## 📥 Installation

You can install the development version of hcruR from [GitHub](https://github.com/mumbarkar/hcruR) with:

``` r
# Install from GitHub (after you upload the repo)
# install.packages("devtools")
devtools::install_github("mumbarkar/hcruR")

# install.packages("pak")
pak::pak("mumbarkar/hcruR")
```

------------------------------------------------------------------------

## 🧪 Example Usage

This is a basic example which shows you how to solve a common problem:

``` r
This is a basic example which shows you how to solve a common problem:

# Load library
library(hcruR)

## Generate HCRU summary using dplyr (this can be used for create HCRU plots)

# Load sample data
data(hcru_sample_data)
data <- hcru_sample_data
head(hcru_sample_data)

# Estimate HCRU
hcru_summary <- estimate_hcru(data,
                          cohort_col = "cohort",
                          patient_id_col = "patient_id",
                          admit_col = "admission_date",
                          discharge_col = "discharge_date",
                          index_col = "index_date",
                          visit_col = "visit_date",
                          encounter_id_col = "encounter_id",
                          setting_col = "care_setting",
                          cost_col = "cost_usd",
                          readmission_col = "readmission",
                          time_window_col = "period",
                          los_col = "length_of_stay",
                          custom_var_list = NULL,
                          pre_days = 180,
                          post_days = 365,
                          readmission_days_rule = 30,
                          group_var_main = "cohort",
                          group_var_by = "care_setting",
                          test = NULL,
                          timeline = "Pre",
                          gt_output = FALSE)

hcru_summary

## Generate HCRU summary using gtsummary (a publication ready output) 

# Estimate HCRU
hcru_summary_gt <- estimate_hcru(data,
                          cohort_col = "cohort",
                          patient_id_col = "patient_id",
                          admit_col = "admission_date",
                          discharge_col = "discharge_date",
                          index_col = "index_date",
                          visit_col = "visit_date",
                          encounter_id_col = "encounter_id",
                          setting_col = "care_setting",
                          cost_col = "cost_usd",
                          readmission_col = "readmission",
                          time_window_col = "period",
                          los_col = "length_of_stay",
                          custom_var_list = NULL,
                          pre_days = 180,
                          post_days = 365,
                          readmission_days_rule = 30,
                          group_var_main = "cohort",
                          group_var_by = "care_setting",
                          test = NULL,
                          timeline = "Pre",
                          gt_output = TRUE)

hcru_summary_gt

## Generate the HCRU plot for average visits by cohort and time-line
# Calculate the average visits
sum_df1 <- hcru_summary$`Summary by settings using dplyr` |>
    dplyr::group_by(
      .data[["time_window"]], 
      .data[["cohort"]], 
      .data[["care_setting"]]) |>
    dplyr::summarise(
      AVG_VISIT = mean(.data[["Visits"]], na.rm = TRUE), .groups = "drop")

# Load the plot_hcru function
p1 <- plot_hcru(
  summary_df = sum_df1,
  x_var = "time_window",
  y_var = "AVG_VISIT",
  cohort_col = "cohort",
  facet_var = "care_setting",
  facet_var_n = 3,
  title = "Average visits by domain and cohort",
  x_label = "Healthcare Setting (Domain)",
  y_label = "Average visit",
  fill_label = "Cohort"
)

p1


## Generate HCRU plot for average cost by cohort and timeline
# Calculate the total cost
df2 <- hcru_summary$`Summary by settings using dplyr` |>
    dplyr::group_by(
      .data[["time_window"]], 
      .data[["cohort"]], 
      .data[["care_setting"]]) |>
    dplyr::summarise(
      AVG_COST = sum(.data[["Cost"]], na.rm = TRUE), .groups = "drop")

p2 <- plot_hcru(
  summary_df = df2,
  x_var = "time_window",
  y_var = "AVG_COST",
  cohort_col = "cohort",
  facet_var = "care_setting",
  facet_var_n = 3,
  title = "Average cost by domain and cohort",
  x_label = "Healthcare Setting (Domain)",
  y_label = "Average cost",
  fill_label = "Cohort"
)

p2
```

------------------------------------------------------------------------

## 🧾 Function Reference

`estimate_hcru()` estimates of healthcare resource utilization (HCRU) from 
electronic health record data across various care settings (e.g., IP, OP, 
ED/ER). It provides descriptive summaries of patient counts, encounters, 
costs, length of stay, and readmission rates for pre- and post-index periods

#### Arguments

| Argument                | Type         | Description                                                                  |
| ----------------------- | ------------ | ---------------------------------------------------------------------------- |
| `data`                  | `data.frame` | Input EHR or claims dataset                                                  |
| `cohort_col`            | `character`  | Column name for cohort group                                                 |
| `patient_id_col`        | `character`  | Column name for patient ID                                                   |
| `admit_col`             | `character`  | Admission/start date column                                                  |
| `discharge_col`         | `character`  | Discharge/end date column                                                    |
| `index_col`             | `character`  | Index or anchor date for each patient                                        |
| `visit_col`             | `character`  | Visit or claim date                                                          |
| `encounter_id_col`      | `character`  | Encounter or claim ID                                                        |
| `setting_col`           | `character`  | Setting type (e.g., "IP", "OP", "ED")                                        |
| `cost_col`              | `character`  | Column for cost data                                                         |
| `readmission_col`       | `character`  | Readmission indicator column                                                 |
| `time_window_col`       | `character`  | "Pre"/"Post" period column                                                   |
| `los_col`               | `character`  | Length of stay column                                                        |
| `custom_var_list`       | `character`  | Additional user-defined metrics (optional)                                   |
| `pre_days`              | `numeric`    | Days before index date (default = 180)                                       |
| `post_days`             | `numeric`    | Days after index date (default = 365)                                        |
| `readmission_days_rule` | `numeric`    | Max days for qualifying readmission (default = 30)                           |
| `group_var_main`        | `character`  | Main grouping variable (default = "cohort")                                  |
| `group_var_by`          | `character`  | Secondary grouping variable (e.g., "care\_setting")                          |
| `test`                  | `list`       | Named list of tests for continuous vars (e.g., `list(cost = "wilcox.test")`) |
| `timeline`              | `character`  | Time window label (e.g., "Pre", "Post")                                      |
| `gt_output`             | `logical`    | Whether to return a formatted `gtsummary` output (default = TRUE)            |
| `...`                   | `...`        | Additional arguments for `gtsummary::tbl_summary()` if `gt_output = TRUE`   |
| `return_type`           | `character`  | Type of output to return: "dplyr" for dplyr summary, "gtsummary" for gtsummary output (default = "dplyr") |


`plot_hcru()` provides the visualization of the events of the 
settings/domains grouped by cohort and time window.

#### Arguments:

| Argument       | Type       | Description |
|----------------|------------|-------------|
| `summary_df`   | dataframe  | Output from `estimate_hcru()` function. |
| `x_var`        | character  | Column name to plot on the x-axis (default `"period"`). |
| `y_var`        | character  | Column name to plot on the y-axis (default `"Cost"`). |
| `cohort_col`   | character  | Name of the column identifying cohorts (default `"cohort"`). |
| `facet_var`    | character  | Column to generate subplots for (default `"care_setting"`). |
| `facet_var_n`  | numeric    | Number of columns in the facet grid (default `3`). |
| `title`        | character  | Title of the plot. |
| `x_lable`      | character  | Label for the x-axis. |
| `y_lable`      | character  | Label for the y-axis. |
| `fill_lable`   | character  | Label for the fill legend. |

------------------------------------------------------------------------

## 📊 Sample Data

This package includes a demo datasets for easy testing:

-   `hcru_sample_data`: 200 patients across 2 cohorts

``` r
head(hcru_sample_data)
```

------------------------------------------------------------------------

## 📚 Vignette

Run the following to access the full walkthrough:

``` r
vignette("hcru-analysis", package = "hcruR")
```

------------------------------------------------------------------------

## 🔬 Use Cases

-   Cost burden studies before/after treatment
-   Resource comparison across patient populations
-   Outcome stratification based on utilization patterns

------------------------------------------------------------------------

## 🛠️ Development

To contribute:

``` bash
git clone https://github.com/mumbarkar/hcruR.git
cd hcruR
```

------------------------------------------------------------------------

## 📜 License

This package is licensed under the **MIT License**.
