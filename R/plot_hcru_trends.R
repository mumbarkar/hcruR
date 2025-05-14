#' Plot HCRU Event Summary
#' @title plot_event_counts
#' @description
#' This plot provide the visualization of the events of the settings/domains
#' grouped by cohort and time window.
#'
#' @param hcru_group_summary Output from estimate_hcru()
#' @param y_var
#' @param x_var
#' @param cohort_col
#' @param time_window
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
plot_event_counts <- function(summary_df,
                              x_var = "period",
                              y_var = "Cost",
                              cohort_col = "cohort",
                              facet_Var = "care_setting",
                              title = "Average total cost by domain and cohort",
                              x_lable = "Healthcare Setting (Domain)",
                              y_lable = "Average total cost",
                              fill_lable = "Cohort"
                              ) {
  p <- ggplot2::ggplot(summary_df,
                       aes(x = .data[[x_var]],
                           y = .data[[y_var]],
                           fill = .data[[cohort_col]],
                           interaction(.data[[facet_Var]], .data[[cohort_col]])
                           )
                       ) +
    ggplot2::geom_bar(stat = "identity",
                      position = position_dodge()
                      ) +
    facet_wrap(~ .data[[facet_Var]], strip.position = "bottom") +
    labs(
      title = title,
      x = x_lable,
      y = y_lable,
      fill = fill_lable
    ) +
    theme(
      strip.placement = "outside",  # Make sure it's outside the plot area
      strip.background = element_blank()  # Optional: remove background
    )

  return(p)
}

