#' @title estimate_hcru
#' @description
#' This function calculates estimates of healthcare resource utilization (HCRU)
#' from electronic health record data across various care settings
#' (e.g., IP, OP, ED/ER). It provides descriptive summaries of patient counts,
#' encounters, costs, length of stay, and readmission rates for pre- and
#' post-index periods
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
#' @param gt_output Logical; if \code{TRUE}, also returns output formatted
#' using \pkg{gtsummary} (default is \code{FALSE}).
#' @param cost_col A character specifying the name of cost column
#' @param los_col A character specifying the name of length of stay column
#' @param readmission_col A character specifying the name of readmission column
#' @param time_window_col A character specifying the name of time window column
#' @param custom_var_list A character vector provides the list of additional
#' columns
#' @param group_var A character specifying the name of grouping column
#' @param test An optional named list of statistical tests
#' (e.g., \code{list(age = "wilcox.test")}).
#'
#' @importFrom dplyr left_join group_by mutate case_when n filter n_distinct
#' @importFrom checkmate assert_data_frame assert_character assert_numeric
#' assert_list check_logical
#'
#' @return A list containing one or two summary data frames:
#' \describe{
#'   \item{Summary by settings using dplyr}{A descriptive summary of HCRU
#'   metrics by cohort, setting, and time window.}
#'   \item{Summary by settings using gtsummary (optional)}{Formatted summary
#'   statistics using \pkg{gtsummary}, if \code{gt_output = TRUE}.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' estimate_hcru(data = hcru_sample_data)
#' }
estimate_hcru <- function(data,
                          cohort_col = "cohort",
                          patient_id_col = "patient_id",
                          admit_col = "admission_date",
                          discharge_col = "discharge_date",
                          index_col = "index_date",
                          visit_col = "visit_date",
                          encounter_id_col = "encounter_id",
                          setting_col = "care_setting",
                          cost_col = "cost_usd",
                          readmission_col = "readmission",
                          time_window_col = "period",
                          los_col = "length_of_stay",
                          custom_var_list = NULL,
                          pre_days = 180,
                          post_days = 365,
                          readmission_days_rule = 30,
                          group_var_main = "cohort",
                          group_var_by = "care_setting",
                          test = NULL,
                          gt_output = FALSE) {
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
  checkmate::assert_character(readmission_col, min.chars = 1)
  checkmate::assert_character(time_window_col, min.chars = 1)
  checkmate::assert_numeric(pre_days)
  checkmate::assert_numeric(post_days)
  checkmate::assert_numeric(readmission_days_rule)
  checkmate::assert_character(cost_col, null.ok = TRUE)
  checkmate::assert_character(group_var_main, null.ok = TRUE)
  checkmate::assert_character(group_var_by, null.ok = TRUE)
  checkmate::assert_list(test, null.ok = TRUE)
  checkmate::check_logical(gt_output)

  # Create a var_list
  if (!is.null(custom_var_list)) {
    var_list <- c(cost_col, los_col, readmission_col, custom_var_list)
  } else {
    var_list <- c(cost_col, los_col, readmission_col)
  }

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
                     pre_days,
                     post_days,
                     readmission_days_rule)

  # Summarize by settings using dplyr
  summary1 <- hcru_data |>
    summarize_descriptives(patient_id_col,
                           setting_col,
                           cohort_col,
                           encounter_id_col,
                           cost_col,
                           los_col,
                           readmission_col,
                           time_window_col)

  # Summarize by settings using gtsummary
  if (gt_output) {
    summary2 <- hcru_data |>
      summarize_descriptives_gt(patient_id_col = patient_id_col,
                                var_list = var_list,
                                group_var_main = group_var_main,
                                group_var_by = group_var_by,
                                test = test)
  }

  # Save output in the list object
  if (gt_output) {
    final_output <- list("Summary by settings using dplyr" = summary1,
                         "Summary by settings using gtsummary" = summary2)
  } else {
    final_output <- list("Summary by settings using dplyr" = summary1)
  }
  return(final_output)
}
