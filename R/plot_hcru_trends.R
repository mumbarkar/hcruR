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
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the hcruR package and sample data
#' library(hcruR)
#' data(hcru_sample_data)
#'
#' # Estimate HCRU summary (dplyr output only)
#' hcru_summary <- estimate_hcru(
#'   data = hcru_sample_data,
#'   cohort_col = "cohort",
#'   patient_id_col = "patient_id",
#'   admit_col = "admission_date",
#'   discharge_col = "discharge_date",
#'   index_col = "index_date",
#'   visit_col = "visit_date",
#'   encounter_id_col = "encounter_id",
#'   setting_col = "care_setting",
#'   cost_col = "cost_usd",
#'   readmission_col = "readmission",
#'   time_window_col = "period",
#'   los_col = "length_of_stay",
#'   gt_output = FALSE
#' )
#'
#' # Summarize average visits by cohort and care setting
#' avg_visits_df <- hcru_summary$`Summary by settings using dplyr` |>
#'   dplyr::group_by(time_window, cohort, care_setting) |>
#'   dplyr::summarise(AVG_VISIT = mean(Visits, na.rm = TRUE), .groups = "drop")
#'
#' # Generate plot
#' plot_hcru(
#'   summary_df = avg_visits_df,
#'   x_var = "time_window",
#'   y_var = "AVG_VISIT",
#'   cohort_col = "cohort",
#'   facet_var = "care_setting",
#'   facet_var_n = 3,
#'   title = "Average Visits by Domain and Cohort",
#'   x_label = "Timeline",
#'   y_label = "Average Visits",
#'   fill_label = "Cohort"
#' )
#' }
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
