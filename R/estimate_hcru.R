#' @title estimate_hcru
#' @description
#' This function calculates an estimates of heath care resource utilization
#' (HCRU) for a given electronic health record data for a given set of settings
#' e.g. IP, OP, ED/ER, etc.
#'
#' @param data A dataframe specifying the health care details
#' @param cohort_col A character specifying the name of the cohort column
#' @param patient_id_col A character specifying the name of the patient
#' identifier column
#' @param admit_col A character specifying the name of the date of admission
#' column
#' @param discharge_col A character specifying the name of the date of
#' discharge column
#' @param index_col A character specifying the name of the index date or
#' diagnosis column
#' @param visit_col A character specifying the name of the date of
#' visit/claim column
#' @param encounter_id_col A character specifying the name of the
#' encounter/claim column
#' @param setting_col A character specifying the name of the HCRU
#' setting column e.g. IP, ED, OP, etc.
#' @param pre_days Number of days before index (default 180)
#' @param post_days Number of days after index (default 365)
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr n
#' @importFrom dplyr filter
#'
#' @returns dataframe with HCRU estimates.
#' @export
#'
#' @examples
estimate_hcru = function(data,
                          cohort_col = "cohort",
                          patient_id_col = "patient_id",
                          admit_col = "admission_date",
                          discharge_col = "discharge_date",
                          index_col = "index_date",
                          visit_col = "visit_date",
                          encounter_id_col = "encounter_id",
                          setting_col = "care_setting",
                          pre_days = 180,
                         post_days = 365) {
  # Primary input checks
  checkmate::assert_data_frame(data, min.rows = 1)

  # Convert date columns into R date formats
  data <- data |>
    dplyr::mutate_at(
      dplyr::vars(admit_col, discharge_col, index_col, visit_col), as.Date
    )

  # Create timeline window
  data <- data |>
    dplyr::mutate(
      period = dplyr::case_when(
        .data[[visit_col]] < .data[[index_col]] ~ "pre",
        .data[[visit_col]] >= .data[[index_col]] ~ "post",
        TRUE ~ NA_character_
      ),
      days = dplyr::case_when(
        period == "pre" ~ as.numeric(.data[[index_col]] - .data[[visit_col]]) + 1,
        period == "post" ~ as.numeric(.data[[visit_col]] - .data[[index_col]]) + 1,
        TRUE ~ NA_real_)
    )

  # Prepare Length of stay (LOS)
  data <- data |>
    dplyr::mutate(
      length_of_stay = as.numeric(discharge_date - admission_date) + 1
    )

  # Readmission logic (IP only)
  data <- data %>%
    arrange(.data[[cohort_col]], .data[[patient_id_col]], admission_date) %>%
    group_by(.data[[cohort_col]], .data[[patient_id_col]], .data[[setting_col]]) %>%
    mutate(next_admit = lead(admission_date),
           days_to_next = as.numeric(next_admit - discharge_date),
           readmit_within_30 = ifelse(.data[[setting_col]] == "IP" & !is.na(days_to_next) & days_to_next <= 30, 1, 0)) %>%
    ungroup()
}
