#' Plot HCRU Event Summary
#' @title plot_hcru
#' @description
#' This function provides the visualization of the events of the settings
#' grouped by cohort and time window.
#'
#' @param summary_df Output from estimate_hcru()
#' @param x_var A character specifying column name to be plotted on x-axis
#' @param y_var A character specifying column name to be plotted on y-axis
#' @param facet_var  A character specifying column name to generate the plot
#' output in columns
#' @param facet_var_n  A numeric specifying number of columns for facet output
#' @param title A character
#' @param x_label A character
#' @param y_label A character
#' @param fill_label A character
#' @param cohort_col A character
#'
#' @import ggplot2
#' @importFrom rlang .data
#'
#' @return ggplot object
#' @export
plot_hcru <- function(
    summary_df,
    x_var = "time_window",
    y_var = "Cost",
    cohort_col = "cohort",
    facet_var = "care_setting",
    facet_var_n = 3,
    title = "Average total cost by domain and cohort",
    x_label = "Healthcare Setting (Domain)",
    y_label = "Average total cost",
    fill_label = "Cohort") {
  # Primary input checks
  checkmate::assert_data_frame(summary_df, min.rows = 1)
  checkmate::assert_character(x_var, min.chars = 1)
  checkmate::assert_character(y_var, min.chars = 1)
  checkmate::assert_character(cohort_col, min.chars = 1)
  checkmate::assert_character(facet_var, min.chars = 1)
  checkmate::assert_number(facet_var_n)
  checkmate::assert_character(title, min.chars = 1)
  checkmate::assert_character(x_label, min.chars = 1)
  checkmate::assert_character(y_label, min.chars = 1)
  checkmate::assert_character(fill_label, min.chars = 1)

  # Character vector explicitly converted to a factor with levels if present
  if (all(c("Pre", "Post") %in% summary_df[[x_var]])) {
    summary_df[[x_var]] <- factor(summary_df[[x_var]],
      levels = c("Pre", "Post")
    )
  } else {
    summary_df[[x_var]] <- as.factor(summary_df[[x_var]])
  }

  p <- ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = .data[[cohort_col]],
      group = .data[[cohort_col]]
    )
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = round(.data[[y_var]], 1),
        group = .data[[cohort_col]]
      ),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = -0.5,
      size = 3.5
    ) +
    ggplot2::facet_wrap(
      ~ .data[[facet_var]],
      strip.position = "top",
      ncol = facet_var_n
    ) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label,
      fill = fill_label
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.border = ggplot2::element_rect(
        color = "black", fill = NA,
        linewidth = 0.8
      )
    )

  return(p)
}
