#' Compare HCRU Between Cohorts
#'
#' @param hcru_summary Output of estimate_hcru()
#' @param cohort Metadata with person_id and cohort_id
#'
#' @return Group-level summary per domain and time window
#' @export
compare_hcru_cohorts <- function(hcru_summary, cohort) {
  hcru_summary <- dplyr::left_join(hcru_summary, cohort, by = "person_id")

  hcru_summary %>%
    dplyr::group_by(cohort_id, domain, time_window) %>%
    dplyr::summarise(
      mean_count = mean(event_count, na.rm = TRUE),
      median_count = median(event_count, na.rm = TRUE),
      mean_cost = mean(total_cost, na.rm = TRUE),
      .groups = "drop"
    )
}
