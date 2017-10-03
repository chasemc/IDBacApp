#' @export
runExample <- function() {
  appDir <- system.file("IDBacApp", "app",package = "IDBac_app")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}