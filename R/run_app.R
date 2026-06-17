#' Launch the Shiny App
#'
#' @importFrom bsicons bs_icon
#' @importFrom bslib tooltip
#' @importFrom dplyr mutate filter
#' @importFrom htmlwidgets JS
#' @importFrom readr read_csv
#' @importFrom rhandsontable rhandsontable hot_to_r hot_col renderRHandsontable rHandsontableOutput hot_table
#' @importFrom rlang .data
#' @importFrom shiny runApp
#' @importFrom shinyjs useShinyjs
#' @importFrom tibble tibble as_tibble
#' @export
run_app <- function() {
  app_dir <- system.file("shiny", "auditstratified", package = "auditstratified")

  if (app_dir == "") {
    app_dir <- "./inst/shiny/auditstratified"
  }

  if (!file.exists(app_dir)) {
    stop("Could not find app directory.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
