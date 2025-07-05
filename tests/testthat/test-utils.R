library(testthat)
library(dplyr)
library(gtsummary)
library(checkmate)
library(hcruR)

# Unit tests for preproc_hcru_fun function
test_that("preproc_hcru_fun works as expected on valid data", {
  data <- tibble::tibble(
    cohort = c("A", "A", "A"),
    patient_id = c(1, 1, 1),
    admission_date = as.Date(c("2020-01-01", "2020-01-20", "2020-05-01")),
    discharge_date = as.Date(c("2020-01-05", "2020-01-25", "2020-05-10")),
    index_date = as.Date(c("2020-01-10", "2020-01-10", "2020-01-10")),
    visit_date = as.Date(c("2020-01-01", "2020-01-20", "2020-05-01")),
    encounter_id = c(100, 101, 102),
    care_setting = c("IP", "IP", "OP")
  )

  result <- preproc_hcru_fun(data)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("visit_days", "period", "length_of_stay", "readmission") %in% colnames(result)))
  expect_equal(result$length_of_stay, c(5, 6, 10))
  # Readmission logic: only for IP, and only if next IP admission is within 30d of discharge
  # The second IP admission is 15 days after the first discharge, so first is readmission=1, second is not followed by another IP within 30d, so readmission=0
  # OP encounters should have NA for readmission
  expect_equal(result$readmission, c(1, 0, NA))
  # Check visit_days and period assignment if logic changed
  expect_true(all(result$visit_days > 0))
  expect_true(all(result$period %in% c("pre", "post")))
})

test_that("preproc_hcru_fun filters out encounters outside pre/post window", {
  data <- tibble::tibble(
    cohort = "A",
    patient_id = 1,
    admission_date = as.Date("2018-01-01"),
    discharge_date = as.Date("2018-01-10"),
    index_date = as.Date("2020-01-01"),
    visit_date = as.Date("2018-01-01"),
    encounter_id = 100,
    care_setting = "IP"
  )

  result <- preproc_hcru_fun(data)
  expect_equal(nrow(result), 0) # Out of pre1 and post1 windows
})

test_that("preproc_hcru_fun assigns visit_days and period correctly", {
  data <- tibble::tibble(
    cohort = "A",
    patient_id = 1,
    admission_date = as.Date("2020-01-01"),
    discharge_date = as.Date("2020-01-02"),
    index_date = as.Date("2020-01-10"),
    visit_date = as.Date("2020-01-09"),
    encounter_id = 100,
    care_setting = "OP"
  )

  result <- preproc_hcru_fun(data)
  expect_equal(result$visit_days, 2)
  expect_equal(result$period, "pre")
})

test_that("readmission logic only applies to IP encounters", {
  data <- tibble::tibble(
    cohort = "B",
    patient_id = 1,
    admission_date = as.Date(c("2020-01-01", "2020-01-20")),
    discharge_date = as.Date(c("2020-01-05", "2020-01-25")),
    index_date = as.Date(c("2020-01-10", "2020-01-10")),
    visit_date = as.Date(c("2020-01-01", "2020-01-20")),
    encounter_id = c(200, 201),
    care_setting = c("OP", "OP") # Not IP
  )

  result <- preproc_hcru_fun(data)
  # For non-IP encounters, readmission should be NA (not 0)
  expect_true(all(is.na(result$readmission)))
})

# unit tests for summarize_descriptives_gt function
# Sample dataset for testing
mock_data <- tibble::tibble(
  USUBJID = rep(1:6, each = 2),
  cohort = rep(c("Control", "Treatment"), 6),
  setting = rep(c("IP", "OP"), 6),
  time_window = rep("pre1", 12),
  var1 = rnorm(12, 10, 2),
  var2 = rnorm(12, 5, 1),
  LOS = rpois(12, 3),
  Readmission = rbinom(12, 1, 0.3)
)

test_that("timeline filter works as expected", {
  mock_filtered <- mock_data
  mock_filtered$time_window[1:6] <- "post"

  result <- summarize_descriptives_gt(
    data = mock_filtered,
    patient_id_col = "USUBJID",
    var_list = c("var1", "var2"),
    group_var_main = "cohort",
    group_var_by = "setting",
    test = NULL,
    timeline = "pre1"
  )
  # Since only half of data is 'pre1', expect smaller table or warning
  expect_s3_class(result, "tbl_merge")
})

test_that("fails on incorrect data input types", {
  expect_error(summarize_descriptives_gt(
    data = list(),
    patient_id_col = "USUBJID",
    var_list = c("var1"),
    group_var_main = "cohort",
    group_var_by = "setting"
  ))
})

# Unit tests for summarize_descriptives function
# Sample data for tests
mock_data1 <- tibble::tibble(
  patient_id = c(1, 1, 2, 2, 3, 3),
  cohort = c("Control", "Control", "Treatment", "Treatment", "Control", "Control"),
  care_setting = c("IP", "IP", "OP", "OP", "IP", "OP"),
  encounter_id = c("a", "b", "c", "d", "e", "f"),
  cost_usd = c(100, 200, 150, 150, 300, 100),
  length_of_stay = c(3, 2, NA, NA, 4, NA),
  readmission = c(1, 0, 0, 0, 1, 0),
  time_window = rep("pre", 6),
  visit_days = c(2, 3, 1, 2, 4, 1)
)

test_that("returns expected output structure", {
  result <- summarize_descriptives(mock_data1)
  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "Days", "Month", "Year", "Visits", "Cost", "LOS", "Readmission",
    "Visit_PPPM", "Visit_PPPY", "Cost_PPPM", "Cost_PPPY"
  ) %in% names(result)))
})

test_that("IP-only variables are NA for non-IP rows", {
  result <- summarize_descriptives(mock_data1)
  non_ip <- result[result$care_setting != "IP", ]
  expect_true(all(is.na(non_ip$LOS)))
  expect_true(all(is.na(non_ip$Readmission)))
})

test_that("computed columns are correct", {
  result <- summarize_descriptives(mock_data1)
  row <- result[result$patient_id == 1 & result$care_setting == "IP", ]
  expect_equal(row$Days, 5)
  expect_equal(row$Month, 5 / 30.417, tolerance = 1e-3)
  expect_equal(row$Year, 5 / 365.5, tolerance = 1e-3)
  expect_equal(row$Visits, 2)
  expect_equal(row$Cost, 300)
  expect_equal(row$LOS, 5)
  expect_equal(row$Readmission, 1)
})

test_that("fails with invalid inputs", {
  expect_error(summarize_descriptives(data = list()))
  expect_error(summarize_descriptives(mock_data, patient_id_col = NULL))
  expect_error(summarize_descriptives(mock_data, cost_col = 123))
})

test_that("summarize_descriptives throws error for invalid input types", {
  expect_error(
    summarize_descriptives("not a dataframe"),
    "Assertion on 'data' failed"
  )
  expect_error(
    summarize_descriptives(data.frame(x = 1), patient_id_col = 123),
    "Assertion on 'patient_id_col' failed"
  )
})
