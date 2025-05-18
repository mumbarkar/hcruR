library(testthat)
library(ggplot2)
library(checkmate)

test_that("plot_hcru returns a ggplot object", {
  sample_df <- data.frame(
    cohort = rep(c("A", "B"), each = 2),
    period = rep(c("pre", "post"), 2),
    Cost = c(100, 200, 150, 300),
    care_setting = rep(c("IP", "OP"), each = 2)
  )

  p <- plot_hcru(summary_df = sample_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot_hcru handles custom labels and layout", {
  sample_df <- data.frame(
    cohort = rep(c("A", "B"), each = 2),
    period = rep(c("pre", "post"), 2),
    Cost = c(100, 200, 150, 300),
    care_setting = rep(c("IP", "OP"), each = 2)
  )

  p <- plot_hcru(
    summary_df = sample_df,
    title = "Custom Title",
    x_lable = "X Axis",
    y_lable = "Y Axis",
    fill_lable = "Cohort Group",
    facet_var_n = 1
  )

  expect_s3_class(p, "ggplot")
})
