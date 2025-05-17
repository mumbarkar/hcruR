#' Plot HCRU Event Summary
#' @title plot_hcru
#' @description
#' This plot provide the visualization of the events of the settings/domains
#' grouped by cohort and time window.
#'
#' @param summary_df Output from estimate_hcru()
#' @param x_var A character specifying column name to be plotted on x-axis
#' @param y_var A character specifying column name to be plotted on y-axis
#' @param facet_var  A character specifying column name to generate the plot
#' output in columns
#' @param facet_var_n  A character specifying column name to generate the plot
#' output in n number of columns
#' @param title A character
#' @param x_lable A character
#' @param y_lable A character
#' @param fill_lable A character
#' @param cohort_col A character
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
plot_hcru <- function(summary_df,
                              x_var = "period",
                              y_var = "Cost",
                              cohort_col = "cohort",
                              facet_var = "care_setting",
                              facet_var_n = 3,
                              title = "Average total cost by domain and cohort",
                              x_lable = "Healthcare Setting (Domain)",
                              y_lable = "Average total cost",
                              fill_lable = "Cohort"
) {

  # Primary input checks
  checkmate::assert_data_frame(summary_df, min.rows = 1)
  checkmate::assert_character(x_var, min.chars = 1)
  checkmate::assert_character(y_var, min.chars = 1)
  checkmate::assert_character(cohort_col, min.chars = 1)
  checkmate::assert_character(facet_var, min.chars = 1)
  checkmate::assert_number(facet_var_n)
  checkmate::assert_character(title, min.chars = 1)
  checkmate::assert_character(x_lable, min.chars = 1)
  checkmate::assert_character(y_lable, min.chars = 1)
  checkmate::assert_character(fill_lable, min.chars = 1)

  # Character vector explicitly converted to a factor with levels
  summary_df[[x_var]] <- factor(summary_df[[x_var]], levels = c("pre", "post"))

  # Generate plot
  p <- ggplot2::ggplot(summary_df,
                       aes(x = .data[[x_var]],
                           y = .data[[y_var]],
                           fill = .data[[cohort_col]],
                           interaction(.data[[facet_var]], .data[[cohort_col]])
                       )) +
    ggplot2::geom_bar(stat = "identity",
                      position = ggplot2::position_dodge()
    ) +
    ggplot2::geom_text(
      aes(label = round(.data[[y_var]], 1)),
      position = position_dodge(width = 0.8),
      vjust = -0.5,       # Move label above the bar
      size = 3.5          # Adjust font size as needed
    ) +
    ggplot2::facet_wrap(~ .data[[facet_var]], strip.position = "bottom", ncol = facet_var_n) +
    ggplot2::labs(
      title = title,
      x = x_lable,
      y = y_lable,
      fill = fill_lable
    ) +
    ggplot2::theme(
      # strip.placement = "outside",  # Make sure it's outside the plot area
      # strip.background = element_blank()  # Optional: remove background
      panel.background = element_blank(),              # Clear panel background
      plot.background = element_blank(),               # Clear outer background
      strip.background = element_blank(),                 # Strip with black border
        # fill = "white", color = "black", linewidth = 0.8),
      strip.placement = "outside",
      strip.text = element_text(face = "bold"),
      panel.border = element_rect(                     # Add black border to facets
        color = "black", fill = NA, linewidth = 0.8)
    )

  return(p)
}

