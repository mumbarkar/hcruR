library(testthat)
library(dplyr)
library(tibble)

test_that("estimate_hcru returns expected output without gt_summary", {
  df <- tibble(
    patient_id = c(1, 2, 2),
    cohort = c("A", "A", "A"),
    encounter_id = 1:3,
    admission_date = as.Date(c("2023-01-01", "2023-01-15", "2023-02-01")),
    discharge_date = as.Date(c("2023-01-03", "2023-01-17", "2023-02-03")),
    index_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    visit_date = as.Date(c("2022-12-30", "2023-01-15", "2023-02-01")),
    care_setting = c("IP", "OP", "IP"),
    cost_usd = c(1000, 200, 500),
    readmission = c(1, 0, 0)
  )

  result <- estimate_hcru(df, gt_output = FALSE)

  expect_type(result, "list")
  expect_named(result, "Summary by settings using dplyr")
  expect_s3_class(result[[1]], "data.frame")
})

test_that("estimate_hcru returns both summaries when gt_output = TRUE", {
  df <- tibble(
    patient_id = c(1, 1, 2),
    cohort = c("A", "A", "B"),
    encounter_id = 1:3,
    admission_date = as.Date(c("2020-01-01", "2020-01-20", "2020-05-01")),
    discharge_date = as.Date(c("2020-01-05", "2020-01-25", "2020-05-10")),
    index_date = as.Date(c("2023-01-01", "2023-01-01", "2023-01-01")),
    visit_date = as.Date(c("2022-12-30", "2023-01-15", "2023-02-01")),
    care_setting = c("IP", "IP", "OP"),
    cost_usd = c(1000, 200, 500),
    readmission = c(1, 0, 0)
  )

  result <- estimate_hcru(df, gt_output = TRUE)

  expect_named(result, c("Summary by settings using dplyr",
                         "Summary by settings using gtsummary"))
  expect_s3_class(result[["Summary by settings using gtsummary"]], "gtsummary")
})

test_that("estimate_hcru throws error for invalid inputs", {
  df <- tibble(patient_id = 1)

  expect_error(
    estimate_hcru("not a dataframe"),
    "Assertion on 'data' failed"
  )

  expect_error(
    estimate_hcru(df, cost_col = 123),
    "Assertion on 'cost_col' failed"
  )
})
