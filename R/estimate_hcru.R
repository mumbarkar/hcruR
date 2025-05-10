#' Estimate HCRU Events and Costs
#'
#' @param cohort Dataframe with person_id and index_date
#' @param hcru Dataframe with person_id, event_date, domain, and cost
#' @param pre_days Number of days before index (default 180)
#' @param post_days Number of days after index (default 365)
#'
#' @return Dataframe with counts and costs per patient per domain and time window
#' @export
estimate_hcru <- function(cohort, hcru, pre_days = 180, post_days = 365) {
  hcru <- dplyr::left_join(hcru, cohort, by = "person_id") |>
    dplyr::mutate(
      relative_day = as.integer(event_date - index_date),
      time_window = dplyr::case_when(
        relative_day < 0 & relative_day >= -pre_days ~ "pre",
        relative_day >= 0 & relative_day <= post_days ~ "post",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(time_window))

  summary <- hcru |>
    dplyr::group_by(person_id, domain, time_window) |>
    dplyr::summarise(
      event_count = dplyr::n(),
      total_cost = sum(cost, na.rm = TRUE),
      .groups = "drop"
    )
  return(summary)
}
