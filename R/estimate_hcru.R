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
#' @param pre_days Number of days before index (default 180 days)
#' @param post_days Number of days after index (default 365 days)
#' @param readmission_days_rule Rule for how many days can be permissible to
#' define readmission criteria in AP setting (default 30 days)
#'
#' @importFrom dplyr left_join group_by summarise dplyr mutate case_when n
#' filter
#' @import checkmate
#' @returns dataframe with HCRU estimates.
#' @export
#'
#' @examples estimate_hcru(data = hcru_sample_data)
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
                         post_days = 365,
                         readmission_days_rule = 30,
                         var_list = c("care_setting", "length_of_stay", "readmission", "cost_usd"),
                         group_var = "cohort",
                         test = NULL) {
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
  checkmate::assert_character(var_list, null.ok = TRUE)
  checkmate::assert_character(group_var, null.ok = TRUE)
  checkmate::assert_character(test, null.ok = TRUE)

  # Pre-process HCRU data
  hcru_data <- data |>
    preproc_hcru_fun(cohort_col,
                     patient_id_col,
                     admit_col,
                     discharge_col,
                     index_col,
                     visit_col,
                     encounter_id_col,
                     setting_col,
                     pre_days ,
                     post_days,
                     readmission_days_rule)

  # Summarize by settings
  summary <- hcru_data |>
    summarize_descriptives(var_list = var_list,
                           group_var = group_var,
                           test = test)

  return(summary)
}
