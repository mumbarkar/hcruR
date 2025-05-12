#' @title preproc_hcru_fun
#' @description
#' This function helps to pre-process the heath care resource utilization
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
#' @param pre_days Number of days before index (default 180 days)
#' @param post_days Number of days after index (default 365 days)
#' @param readmission_days_rule Rule for how many days can be permissible to
#' define readmission criteria in AP setting (default 30 days)
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
preproc_hcru_fun = function(data,
                            cohort_col = "cohort",
                            patient_id_col = "patient_id",
                            admit_col = "admission_date",
                            discharge_col = "discharge_date",
                            index_col = "index_date",
                            visit_col = "visit_date",
                            encounter_id_col = "encounter_id",
                            setting_col = "care_setting",
                            pre_days = 180,
                            post_days = 365,
                            readmission_days_rule = 30) {
  # Primary input checks
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_character(cohort_col, min.chars = 1)
  checkmate::assert_character(patient_id_col, min.chars = 1)
  checkmate::assert_character(admit_col, min.chars = 1)
  checkmate::assert_character(discharge_col, min.chars = 1)
  checkmate::assert_character(index_col, min.chars = 1)
  checkmate::assert_character(visit_col, min.chars = 1)
  checkmate::assert_character(encounter_id_col, min.chars = 1)
  checkmate::assert_character(setting_col, min.chars = 1)
  checkmate::assert_numeric(pre_days)
  checkmate::assert_numeric(post_days)
  checkmate::assert_numeric(readmission_days_rule)

  # Convert date columns into R date formats
  data <- data |>
    dplyr::mutate_at(
      dplyr::vars(admit_col, discharge_col, index_col, visit_col), as.Date
    )

  # Mutate visit_days and period columns
  data <- data |>
    dplyr::mutate(
      visit_days = dplyr::case_when(
        .data[[visit_col]] < .data[[index_col]] ~ as.numeric(
          .data[[index_col]] - .data[[visit_col]]
        ) + 1,
        .data[[visit_col]] >= .data[[index_col]] ~ as.numeric(
          .data[[visit_col]] - .data[[index_col]]
        ) + 1,
        TRUE ~ NA_real_
      ),
      period = dplyr::case_when(
        .data[[visit_col]] < .data[[index_col]] ~ "pre",
        .data[[visit_col]] >= .data[[index_col]] ~ "post",
        TRUE ~ NA_character_
      )
    )

  # Create timeline window
  data <- data |>
    dplyr::mutate(
      time_window = dplyr::case_when(
        .data[["period"]] == "pre" & .data[["visit_days"]] >=0 & .data[["visit_days"]] <= pre_days ~ "pre1",
        .data[["period"]] == "post" & .data[["visit_days"]] >=0 & .data[["visit_days"]] <= post_days ~ "post1",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(time_window)) |>
    dplyr::select(-time_window)

  # Prepare Length of stay (LOS)
  data <- data |>
    dplyr::mutate(
      length_of_stay = as.numeric(.data[[discharge_col]] - .data[[admit_col]]) + 1
    )

  # Readmission logic (IP only)
  final_data <- data |>
    dplyr::arrange(.data[[cohort_col]], .data[[patient_id_col]], .data[[admit_col]]) |>
    dplyr::group_by(.data[[cohort_col]], .data[[patient_id_col]], .data[[setting_col]]) |>
    dplyr::mutate(next_admit = dplyr::lead(.data[[admit_col]]),
           days_to_next = as.numeric(.data[["next_admit"]] - .data[[discharge_col]]),
           readmission = ifelse(
             .data[[setting_col]] == "IP" &
               !is.na(.data[["days_to_next"]]) &
               .data[["days_to_next"]] <= as.numeric(readmission_days_rule), 1, 0)
           ) |>
    dplyr::ungroup()

  return(final_data)
}
