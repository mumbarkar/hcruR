#' Sample Cohort Data
#'
#' A sample dataset representing a patient cohort with index dates.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{person_id}{Unique patient identifier}
#'   \item{cohort_id}{Cohort identifier (e.g., treatment group)}
#'   \item{index_date}{Index date (as Date)}
#' }
#' @source Simulated data
"cohort_sample"

#' Sample Raw HCRU Events
#'
#' A sample dataset of healthcare resource utilization (HCRU) events.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{person_id}{Unique patient identifier}
#'   \item{event_date}{Date of the healthcare event}
#'   \item{domain}{Type of healthcare domain (e.g., inpatient, outpatient)}
#'   \item{cost}{Cost associated with the event}
#' }
#' @source Simulated data
"raw_hcru"
