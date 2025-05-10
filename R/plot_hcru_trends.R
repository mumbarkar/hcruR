#' Plot HCRU Summary
#'
#' @param hcru_group_summary Output from compare_hcru_cohorts()
#' @param metric Either 'mean_count' or 'mean_cost'
#'
#' @import ggplot2
#'
#' @return ggplot object
#' @export
plot_hcru <- function(hcru_group_summary, metric = "mean_count") {
  ggplot2::ggplot(hcru_group_summary,
                  ggplot2::aes_string(x = "domain", y = metric, fill = "factor(cohort_id)")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~ time_window) +
    ggplot2::labs(
      title = paste("HCRU by Domain and Time Window (", metric, ")", sep = ""),
      fill = "Cohort"
    ) +
    ggplot2::theme_minimal()
}
