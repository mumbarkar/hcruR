#' Estimate HCRU Events and Costs
#'
#' @param cohort Dataframe with person_id and index_date
#' @param hcru Dataframe with person_id, event_date, domain, and cost
#' @param pre_days Number of days before index (default 180)
#' @param post_days Number of days after index (default 365)
#'
#' @import dplyr
#' @importFrom dplyr lag
#' @importFrom dplyr filter
#' @import stats
#'
#' @return Dataframe with counts and costs per patient per domain and time window
#' @export
estimate_hcru <- function(cohort, hcru, pre_days = 180, post_days = 365) {
  hcru <- left_join(hcru, cohort, by = "person_id") |>
    mutate(
      relative_day = as.integer(.data[["event_date"]] - .data[["index_date"]]),
      time_window = case_when(
        .data[["relative_day"]] < 0 & .data[["relative_day"]] >= pre_days ~ "pre",
        .data[["relative_day"]] >= 0 & .data[["relative_day"]] <= post_days ~ "post",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data[["time_window"]]))

  summary <- hcru |>
    group_by(.data[["person_id"]], .data[["domain"]], .data[["time_window"]]) |>
    summarise(
      event_count = n(),
      total_cost = sum(.data[["cost"]], na.rm = TRUE),
      .groups = "drop"
    )
  return(summary)
}
