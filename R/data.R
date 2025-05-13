#' Sample Cohort Data
#'
#' A sample dataset representing a patient cohort with index dates.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{patient_id}{Unique patient identifier}
#'   \item{cohort}{Cohort identifier (e.g., treatment group)}
#'   \item{index_date}{Index date (as Date)}
#'   \item{encounter_id}{encounter/claim identifier (e.g., claim number)}
#'   \item{care_setting}{HCRU domain types (e.g., IP, OP, ER, etc.)}
#'   \item{visit_date}{Visit date (as Date)}
#'   \item{admission_date}{Admission date (as Date)}
#'   \item{discharge_date}{Discharge date (as Date)}
#'   \item{encounter_date}{Encounter/Claim date (as Date)}
#'   \item{period}{period (e.g., Pre/Post)}
#'   \item{cost_usd}{Cost of utilization of health resources}
#' }
#' @source Simulated data
"hcru_sample_data"
