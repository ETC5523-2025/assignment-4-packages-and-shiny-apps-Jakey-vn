#' Launch the HAI Dashboard (German PPS 2011)
#'
#' Starts the Shiny application bundled with the package. The app provides
#' an interactive overview of healthcare-associated infections (HAIs) from
#' the German point-prevalence survey (PPS) 2011, including total cases by
#' infection type and distributions of length of infection.
#'
#' @details
#' @section Tabs:
#' \itemize{
#'   \item \strong{Cases (interactive)} – Bar chart of HAI cases by type with
#'   hover tooltips showing the standardized rate per 1,000 survey patients.
#'   \item \strong{Length of Infection} – Histogram of infection durations (days)
#'   for the selected infection type.
#'   \item \strong{About \& How to Read} – Plain-language descriptions of fields,
#'   definitions, and interpretation guidance.
#' }
#'
#' @section Top metrics:
#' \itemize{
#'   \item \emph{Survey Patients}: total patients included in the PPS snapshot.
#'   \item \emph{Population (2011)}: national population (context).
#'   \item \emph{Mean Length of Stay}: average hospital stay in days.
#' }
#'
#' @section Data sources:
#' The app uses objects from the \pkg{BHAI} example dataset
#' \code{german_pps_2011_repr} at runtime:
#' \code{num_survey_patients}, \code{population}, \code{length_of_stay},
#' \code{num_hai_patients}, \code{loi_pps}. A convenience table
#' \code{df_cases} (cases and rate per 1,000) is derived inside the app.
#' If you instead ship precomputed data objects in \pkg{AndyWebsite}, see
#' \code{?df_cases}, \code{?metrics}, and \code{?loi_by_type}.
#'
#' @return Opens the Shiny app in your browser.
#'
#' @examples
#' \dontrun{
#'   launch_app()
#' }
#'
#' @export
#' @import shiny
#' @import plotly
#' @import bslib
#' @import ggplot2
#' @import dplyr
#' @import BHAI
launch_app <- function() {
  app_dir <- system.file("app", package = "AndyWebsite")
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Could not find app directory. Reinstall the package or check inst/app/.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}