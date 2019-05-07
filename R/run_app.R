#' run_app
#'
#' @return NA
#' @export
#'

run_app <- function(port = 9191) {
  
  shiny::shinyApp(ui = IDBacApp::app_ui(),
                  server = IDBacApp::app_server,
                  options = list(port = 9191))
}