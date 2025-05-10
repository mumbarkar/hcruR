test_that("compare_hcru_cohorts computes summary statistics by cohort", {
  hcru_summary <- tibble::tibble(
    person_id = c(1, 2, 3, 4),
    domain = c("ip", "ip", "ed", "ed"),
    time_window = c("pre", "pre", "post", "post"),
    event_count = c(1, 2, 3, 4),
    total_cost = c(100, 200, 300, 400)
  )

  cohort <- tibble::tibble(
    person_id = c(1, 2, 3, 4),
    cohort_id = c("A", "A", "B", "B")
  )

  result <- compare_hcru_cohorts(hcru_summary, cohort)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("cohort_id", "domain", "time_window", "mean_count", "median_count", "mean_cost") %in% names(result)))
  expect_equal(nrow(result), 2)  # 2 cohort/domain/time_window combos
  expect_equal(result$mean_cost[result$cohort_id == "A"], mean(c(100, 200)))
})

test_that("compare_hcru_cohorts handles NA values", {
  hcru_summary <- tibble::tibble(
    person_id = c(1, 2),
    domain = c("ip", "ip"),
    time_window = c("post", "post"),
    event_count = c(1, NA),
    total_cost = c(100, NA)
  )

  cohort <- tibble::tibble(
    person_id = c(1, 2),
    cohort_id = c("A", "A")
  )

  result <- compare_hcru_cohorts(hcru_summary, cohort)

  expect_equal(result$mean_count, mean(c(1, NA), na.rm = TRUE))
  expect_equal(result$median_count, median(c(1, NA), na.rm = TRUE))
  expect_equal(result$mean_cost, 100)
})

