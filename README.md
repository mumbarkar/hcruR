
# hcruR

<!-- badges: start -->
<!-- badges: end -->

The goal of hcruR is to to estimate and compare healthcare resource utilization (HCRU) by domain and cohort. Supports cohort-based event counting, cost summarization, time window analysis, and plotting for RWE studies.

## Installation

You can install the development version of hcruR from [GitHub](https://github.com/mumbarkar/hcruR) with:

``` r
# install.packages("pak")
pak::pak("mumbarkar/hcruR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(hcruR)

## basic example code
summary <- estimate_hcru(cohort_sample, raw_hcru)
group_summary <- compare_hcru_cohorts(summary, cohort_sample)
plot_hcru(group_summary, metric = "mean_count")
```

