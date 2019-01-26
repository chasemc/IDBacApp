run_app <- function() {
  shiny::shinyApp(ui = IDBacApp::app_ui(), server = IDBacApp::app_server)
}