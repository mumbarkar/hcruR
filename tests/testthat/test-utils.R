library(testthat)
library(dplyr)

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
  expect_equal(result$readmission, c(1, 0, 0))  # First IP has a readmission within 30 days
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
  expect_equal(nrow(result), 0)  # Out of pre1 and post1 windows
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
  expect_equal(result$visit_days, 2) # 10 - 9 + 1
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
  expect_equal(result$readmission, c(0, 0))  # No IP → no readmission logic triggered
})

test_that("preproc_hcru_fun throws error for incorrect input types", {
  expect_error(preproc_hcru_fun("not a dataframe"), "Assertion on 'data' failed")
  expect_error(preproc_hcru_fun(data.frame(x=1), pre_days = "180"), "Assertion on 'pre_days' failed")
  expect_error(preproc_hcru_fun(data.frame(x=1), post_days = "365"), "Assertion on 'post_days' failed")
  expect_error(preproc_hcru_fun(data.frame(x=1), readmission_days_rule = "30"), "Assertion on 'readmission_days_rule' failed")
})

# Unit tests for summarize_descriptives_gtsummary function
test_that("summarize_descriptives_gtsummary returns expected object without grouping", {
  df <- data.frame(
    age = c(50, 60, 70),
    sex = c("M", "F", "F")
  )

  tbl <- summarize_descriptives_gtsummary(data = df, var_list = c("age", "sex"))

  expect_s3_class(tbl, "gtsummary")
  expect_true("tbl" %in% class(tbl$table_body))
})

test_that("summarize_descriptives_gtsummary works with grouping variable", {
  df <- data.frame(
    age = c(50, 60, 70),
    sex = c("M", "F", "F"),
    group = c("A", "A", "B")
  )

  tbl <- summarize_descriptives_gtsummary(data = df, var_list = c("age", "sex"), group_var = "group")

  expect_s3_class(tbl, "gtsummary")
  expect_true(any(grepl("p.value", names(tbl$table_body))))  # Expect p-value column
})

test_that("summarize_descriptives_gtsummary handles missing group_var gracefully", {
  df <- data.frame(
    age = c(50, 60, 70),
    sex = c("M", "F", "F")
  )

  tbl <- summarize_descriptives_gtsummary(data = df, var_list = c("age", "sex"), group_var = NULL)

  expect_s3_class(tbl, "gtsummary")
})

test_that("summarize_descriptives_gtsummary throws error on invalid input", {
  expect_error(summarize_descriptives_gtsummary("not a dataframe"), "Assertion on 'data' failed")
  expect_error(summarize_descriptives_gtsummary(data.frame(x = 1), var_list = 1), "Assertion on 'var_list' failed")
  expect_error(summarize_descriptives_gtsummary(data.frame(x = 1), group_var = 1), "Assertion on 'group_var' failed")
})

# Unit tests for summarize_descriptives function
test_that("summarize_descriptives produces correct summary for mixed settings", {
  df <- tibble::tibble(
    patient_id = c(1, 1, 2, 3, 3),
    care_setting = c("IP", "OP", "IP", "IP", "OP"),
    cohort = c("A", "A", "A", "B", "B"),
    encounter_id = c(101, 102, 103, 104, 105),
    cost_usd = c(1000, 200, 1500, 3000, 500),
    length_of_stay = c(5, NA, 4, 7, NA),
    readmission = c(1, NA, 0, 1, NA),
    period = c("pre", "pre", "post", "post", "post")
  )

  result <- summarize_descriptives(df)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("Patients", "Visits", "Cost", "Avg_visits_per_patient",
                    "Avg_cost_per_patient", "Avg_LOS", "Readmit_30d_Rate") %in% names(result)))

  # Spot-check a group
  ip_b_post <- result |>
    filter(cohort == "B", care_setting == "IP", period == "post")

  expect_equal(ip_b_post$Patients, 1)
  expect_equal(ip_b_post$Visits, 1)
  expect_equal(ip_b_post$Avg_LOS, 7)
  expect_equal(ip_b_post$Readmit_30d_Rate, 100)
})

test_that("summarize_descriptives omits LOS and Readmission Rate for non-IP settings", {
  df <- tibble::tibble(
    patient_id = 1:3,
    care_setting = rep("OP", 3),
    cohort = "A",
    encounter_id = 101:103,
    cost_usd = c(100, 200, 300),
    length_of_stay = NA,
    readmission = NA,
    period = "pre"
  )

  result <- summarize_descriptives(df)
  expect_true(is.na(result$Avg_LOS))
  expect_true(is.na(result$Readmit_30d_Rate))
})

test_that("summarize_descriptives handles NA cost and readmission", {
  df <- tibble::tibble(
    patient_id = c(1, 2),
    care_setting = c("IP", "IP"),
    cohort = "X",
    encounter_id = c(1, 2),
    cost_usd = c(NA, 200),
    length_of_stay = c(3, 4),
    readmission = c(1, NA),
    period = "pre"
  )

  result <- summarize_descriptives(df)

  expect_equal(result$Cost, 200)
  expect_equal(result$Avg_LOS, 3.5)
  expect_equal(result$Readmit_30d_Rate, 100)  # 1 / 1 (non-NA) * 100
})

test_that("summarize_descriptives throws error for invalid input types", {
  expect_error(summarize_descriptives("not a dataframe"), "Assertion on 'data' failed")
  expect_error(summarize_descriptives(data.frame(x = 1), patient_id_col = 123), "Assertion on 'patient_id_col' failed")
})
