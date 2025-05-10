test_that("estimate_hcru works", {
  out <- estimate_hcru(cohort_sample, raw_hcru)
  expect_s3_class(out, "data.frame")
  expect_true("event_count" %in% names(out))
})

test_that("estimate_hcru works for typical inputs", {
  cohort <- tibble::tibble(
    person_id = c(1, 2),
    index_date = as.Date(c("2020-01-01", "2020-06-01"))
  )

  hcru <- tibble::tibble(
    person_id = c(1, 1, 2, 2),
    event_date = as.Date(c("2019-08-01", "2020-02-01", "2020-05-15", "2020-08-01")),
    domain = c("ip", "op", "ip", "ed"),
    cost = c(100, 200, 150, 250)
  )

  result <- estimate_hcru(cohort, hcru)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("person_id", "domain", "time_window", "event_count", "total_cost") %in% names(result)))
  expect_equal(nrow(result), 2)  # One event is outside the pre/post window
})

test_that("estimate_hcru excludes events outside time window", {
  cohort <- tibble::tibble(
    person_id = 1,
    index_date = as.Date("2020-01-01")
  )

  hcru <- tibble::tibble(
    person_id = 1,
    event_date = as.Date("2018-01-01"),  # way before pre_days
    domain = "ip",
    cost = 100
  )

  result <- estimate_hcru(cohort, hcru)
  expect_equal(nrow(result), 0)
})

test_that("estimate_hcru handles missing cost gracefully", {
  cohort <- tibble::tibble(
    person_id = 1,
    index_date = as.Date("2020-01-01")
  )

  hcru <- tibble::tibble(
    person_id = 1,
    event_date = as.Date("2020-02-01"),
    domain = "ed",
    cost = NA
  )

  result <- estimate_hcru(cohort, hcru)
  expect_equal(result$total_cost, 0)
  expect_equal(result$event_count, 1)
})
