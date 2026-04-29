#' Launch the Shiny App
#'
#' @importFrom shiny runApp
#' @importFrom readr read_csv
#' @importFrom dplyr mutate filter
#' @importFrom tibble tibble as_tibble
#' @importFrom htmlwidgets JS
#' @importFrom rhandsontable rhandsontable hot_to_r hot_col renderRHandsontable rHandsontableOutput hot_table
#' @importFrom rlang .data
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
