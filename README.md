# hcruR <img src="man/figures/hcruR.png" align="right" height="130"/>
<!-- badges: start -->
[![R-CMD-check](https://github.com/mumbarkar/hcruR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mumbarkar/hcruR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/mumbarkar/hcruR/graph/badge.svg)](https://app.codecov.io/gh/mumbarkar/hcruR)
[![GitHub version](https://img.shields.io/github/v/release/mumbarkar/hcruR)](https://github.com/mumbarkar/hcruR/releases)
[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->
  
**hcruR** is an R package to help health economists and RWE analysts estimate and compare healthcare resource utilization (HCRU) from observational healthcare data, such as claims or electronic health records.

------------------------------------------------------------------------

##  🚀 Features

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
library(hcruR)

# Load sample data
data(hcru_sample_data)

# Step 1: Estimate HCRU
hcru_summary = estimate_hcru(data = hcru_sample_data,
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
                             group_var = "cohort",
                             test = NULL,
                             gt_output = FALSE)
# Step 3: Plot results
p = plot_hcru(summary_df = hcru_summary$`Summary by settings using dplyr`,
              x_var = "period",
              y_var = "Cost",
              cohort_col = "cohort",
              facet_var = "care_setting",
              facet_var_n = 3,
              title = "Average total cost by domain and cohort",
              x_lable = "Healthcare Setting (Domain)",
              y_lable = "Average total cost",
              fill_lable = "Cohort"
)

p
```

------------------------------------------------------------------------

## 🧾 Function Reference

`estimate_hcru()` Estimate patient-level HCRU counts and total costs.

Arguments:

-   `cohort`: Data frame with `person_id` and `index_date`

-   `hcru`: Data frame with `person_id`, `event_date`, `domain`, and `cost`

-   `pre_days`: Number of days before index date to count events

-   `post_days`: Number of days after index date to count events

`compare_hcru_cohorts()` Summarizes HCRU statistics by cohort.

Arguments:

-   `hcru_summary`: Output from `estimate_hcru()`

-   `cohort`: Must contain `cohort_id` to enable comparisons

`plot_hcru()` Visualizes mean count or cost per domain, by time period and cohort.

Arguments:

-   `hcru_group_summary`: Output from `compare_hcru_cohorts()`
-   `metric`: `"mean_count"` or `"mean_cost"`

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
