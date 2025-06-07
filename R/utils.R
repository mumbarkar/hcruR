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
#' @importFrom dplyr select left_join group_by mutate case_when n
#' filter
#' @import checkmate
#'
#' @returns dataframe with HCRU estimates.
#' @export
#'
#' @examples preproc_hcru_fun(data = hcru_sample_data)
preproc_hcru_fun <- function(data,
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
        .data[["period"]] == "pre" & .data[["visit_days"]] >= 0 &
          .data[["visit_days"]] <= pre_days ~ "pre1",
        .data[["period"]] == "post" & .data[["visit_days"]] >= 0 &
          .data[["visit_days"]] <= post_days ~ "post1",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(.data[["time_window"]]))

  # Prepare Length of stay (LOS)
  data <- data |>
    dplyr::mutate(
      length_of_stay = as.numeric(
        .data[[discharge_col]] - .data[[admit_col]]) + 1)

  # Readmission logic (IP only)
  final_data <- data |>
    dplyr::arrange(
      .data[[cohort_col]], .data[[patient_id_col]], .data[[admit_col]]
    ) |>
    dplyr::group_by(
      .data[[cohort_col]], .data[[patient_id_col]], .data[[setting_col]]
    ) |>
    dplyr::mutate(next_admit = dplyr::lead(
      .data[[admit_col]]
    ),
    days_to_next = as.numeric(
      .data[["next_admit"]] - .data[[discharge_col]]
    )) |>
    dplyr::ungroup() |>
    dplyr::mutate(
    readmission = dplyr::case_when(
      .data[[setting_col]] == "IP" & !is.na(.data[["days_to_next"]]) &
        .data[["days_to_next"]] <= as.numeric(readmission_days_rule) ~ 1,
      (.data[[setting_col]] == "IP" & is.na(.data[["days_to_next"]])) |
        (.data[[setting_col]] == "IP" & !is.na(.data[["days_to_next"]]) &
           .data[["days_to_next"]] > as.numeric(readmission_days_rule)) ~ 0,
      TRUE ~ NA_integer_)
    )

  return(final_data)
}

#' Generate Detailed Descriptive Statistics with Custom P-Value Tests
#'
#' @param data A dataframe with variables to summarize.
#' @param var_list Optional quoted variable list (e.g. care_setting).
#' @param group_var Optional quoted grouping variable (e.g. cohort).
#' @param test Optional named list of statistical tests
#' (e.g. age ~ "wilcox.test").
#'
#' @importFrom gtsummary tbl_summary add_overall add_p modify_header add_n
#' modify_spanning_header all_continuous all_categorical all_stat_cols
#' @import checkmate
#' @import glue
#'
#' @return A gtsummary table object
#' @export
#'
summarize_descriptives_gt <- function(data,
                                      patient_id_col,
                                      var_list = NULL,
                                      group_var_main = NULL,  # e.g., "cohort"
                                      group_var_by = NULL,    # e.g., "setting"
                                      test = NULL) {

  # Basic checks
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_character(var_list, null.ok = TRUE)
  checkmate::assert_character(group_var_main, len = 1)
  checkmate::assert_character(group_var_by, len = 1)
  checkmate::assert_list(test, null.ok = TRUE)

  # Unique settings (IP, OP, ED...)
  settings <- unique(data[[group_var_by]])

  # Iterate by setting
  setting_tbls <- purrr::map(settings, function(setting) {
    df_setting <- dplyr::filter(data, .data[[group_var_by]] == setting)

    # Drop IP-only vars if not IP
    vars_this <- var_list
    if (setting != "IP") {
      vars_this <- setdiff(vars_this, c("length_of_stay", "readmission"))
    }

    # Cohort group (control, treatment, etc.)
    cohort_sym <- rlang::sym(group_var_main)

    # Get N per group
    n_df <- df_setting |>
      dplyr::distinct(.data[[patient_id_col]], .data[[group_var_main]]) |>
      dplyr::count(.data[[group_var_main]], name = "n") |>
      dplyr::mutate(
        colname = paste0("stat_", dplyr::row_number()),
        header = glue::glue("{.data[[group_var_main]]}, N = {n}")
      )
    header_map <- stats::setNames(n_df$header, n_df$colname)

    # Build summary
    tbl <- gtsummary::tbl_summary(
      data = df_setting,
      by = !!cohort_sym,
      include = all_of(vars_this),
      missing = "no",
      type = list(all_continuous() ~ "continuous2"),
      statistic = list(
        all_continuous() ~ c(
          "Mean (SD)" = "{mean} ({sd})",
          "Median (IQR)" = "{median} ({p25}, {p75})",
          "Q1" = "{p25}",
          "Q3" = "{p75}",
          "Range" = "{min} - {max}"
        )
      )
    )

    if (length(unique(df_setting[[group_var_main]])) > 1) {
      tbl <- if (!is.null(test)) {
        tbl |> gtsummary::add_p(test = test)
      } else {
        tbl |> gtsummary::add_p()
      }
    }

    tbl <- tbl |>
      gtsummary::modify_header(update = header_map) |>
      gtsummary::modify_caption(glue::glue("**Summary Table**")) |>
      gtsummary::bold_labels()

    return(tbl)
  })

  # Merge all setting-wise cohort tables side-by-side
  gtsummary::tbl_merge(
    tbls = setting_tbls,
    tab_spanner = settings
  )
}


#' Generate Detailed Descriptive Statistics
#'
#' @param data A dataframe with variables to summarize.
#' @param patient_id_col A character specifying the name of patient
#' identifier column
#' @param setting_col A character specifying the name of HRCU setting column
#' @param cohort_col A character specifying the name of cohort column
#' @param encounter_id_col A character specifying the name of encounter/claim
#' column
#' @param cost_col A character specifying the name of cost column
#' @param los_col A character specifying the name of length of stay column
#' @param readmission_col A character specifying the name of readmission column
#' @param time_window_col A character specifying the name of time window column
#'
#' @importFrom dplyr select left_join group_by reframe mutate case_when n
#' filter ungroup n_distinct if_else distinct
#' @import checkmate
#'
#' @return A table object
#' @export
#'
summarize_descriptives <- function(data,
                                   patient_id_col = "patient_id",
                                   setting_col = "care_setting",
                                   cohort_col = "cohort",
                                   encounter_id_col = "encounter_id",
                                   cost_col = "cost_usd",
                                   los_col = "length_of_stay",
                                   readmission_col = "readmission",
                                   time_window_col = "period") {

  # Primary input checks
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_character(patient_id_col, min.chars = 1)
  checkmate::assert_character(cohort_col, min.chars = 1)
  checkmate::assert_character(encounter_id_col, min.chars = 1)
  checkmate::assert_character(setting_col, min.chars = 1)
  checkmate::assert_character(readmission_col, min.chars = 1)
  checkmate::assert_character(time_window_col, min.chars = 1)
  checkmate::assert_character(cost_col, min.chars = 1)
  checkmate::assert_character(los_col, min.chars = 1)
  checkmate::assert_character(readmission_col, min.chars = 1)
  checkmate::assert_character(time_window_col, min.chars = 1)

  # Generate summary
  summary_df <- data |>
    dplyr::group_by(
      .data[[cohort_col]], .data[[setting_col]], .data[[time_window_col]]) |>
    dplyr::reframe(
      Patients = dplyr::n_distinct(.data[[patient_id_col]]),
      Visits = dplyr::n_distinct(.data[[encounter_id_col]]),
      Cost = sum(.data[[cost_col]], na.rm = TRUE),
      Avg_visits_per_patient = round(.data[["Visits"]] / .data[["Patients"]],
                                     2),
      Avg_cost_per_patient = round(.data[["Cost"]] / .data[["Patients"]], 2),
      Avg_LOS = dplyr::if_else(.data[[setting_col]] == "IP",
                               round(mean(.data[[los_col]], na.rm = TRUE), 2),
                               NA_real_),
      Readmit_30d_Rate = dplyr::if_else(.data[[setting_col]] == "IP",
                                        round(mean(.data[[readmission_col]],
                                                   na.rm = TRUE) * 100, 2),
                                        NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  return(summary_df)
}
