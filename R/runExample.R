#' @export
runExample <- function() {
  appDir <- "C:/Users/chase/Documents/R/win-library/3.4/IDBacApp/app"
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
