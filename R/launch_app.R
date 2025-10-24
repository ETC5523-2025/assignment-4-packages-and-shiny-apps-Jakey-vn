#' Launch the Shiny App
#' @export
launch_app <- function() {
  app_dir <- system.file("app", package = "AndyWebsite")
  shiny::runApp(app_dir)
}