library(testthat)
library(ggplot2)
library(dplyr)
library(hcruR)

# Sample summarized data
mock_summary_df <- tibble::tibble(
  time_window = rep(c("pre1", "post1"), each = 4),
  Cost = c(100, 150, 200, 180, 110, 160, 190, 170),
  cohort = rep(c("Control", "Treatment"), times = 4),
  care_setting = rep(c("IP", "OP"), times = 4)
)

test_that("returns a ggplot object", {
  p <- plot_hcru(mock_summary_df)
  expect_s3_class(p, "ggplot")
})

test_that("plot contains correct labels and facets", {
  p <- plot_hcru(mock_summary_df, title = "Test Title", x_label = "Time", y_label = "Cost", fill_label = "Group")
  built <- ggplot_build(p)
  expect_true("Test Title" %in% p$labels$title)
  expect_true("Time" %in% p$labels$x)
  expect_true("Cost" %in% p$labels$y)
  expect_true("Group" %in% p$labels$fill)
  expect_equal(length(unique(mock_summary_df$care_setting)), length(built$layout$layout$PANEL))
})

test_that("works with non-standard time_window values", {
  df <- mock_summary_df
  df$time_window <- rep(c("baseline", "followup"), each = 4)
  p <- plot_hcru(df)
  expect_s3_class(p, "ggplot")
})
