test_that("estimate_hcru works", {
  out <- estimate_hcru(cohort_sample, raw_hcru)
  expect_s3_class(out, "data.frame")
  expect_true("event_count" %in% names(out))
})
