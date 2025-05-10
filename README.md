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
# install.packages("pak")
pak::pak("mumbarkar/hcruR")
```

------------------------------------------------------------------------

## 🧪 Example Usage

This is a basic example which shows you how to solve a common problem:

``` r
library(hcruR)

# Load sample data
data(cohort_sample)
data(raw_hcru)

# Step 1: Estimate patient-level HCRU
patient_summary <- estimate_hcru(
  cohort = cohort_sample,
  hcru = raw_hcru,
  pre_days = 180,
  post_days = 365
)

# Step 2: Compare cohorts (if cohort_id is present)
group_summary <- compare_hcru_cohorts(http://127.0.0.1:9631/graphics/plot_zoom_png?width=1188&height=596
  hcru_summary = patient_summary,
  cohort = cohort_sample
)

# Step 3: Plot results
plot_hcru(group_summary, metric = "mean_cost")
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

This package includes two demo datasets for easy testing:

-   `cohort_sample`: 4 patients across 2 cohorts

-   `raw_hcru`: Example HCRU records across multiple domains

``` r
head(cohort_sample)
head(raw_hcru)
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
