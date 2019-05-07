#' run_app
#'
#' @param port The TCP port that the application should listen on
#'
#' @return NA
#' @export
#'

run_app <- function(port = 9191) {
  
  shiny::shinyApp(ui = IDBacApp::app_ui(),
                  server = IDBacApp::app_server,
                  options = list(port = port))
}