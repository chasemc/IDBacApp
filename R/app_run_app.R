#' run_app
#'
#' @param port The TCP port that the application should listen on
#'    if NULL, a random one is assigned
#' @param ... dots!
#'
#' @return NA
#' @export
#' @import shiny
run_app <- function(port = NULL, ...) {
  options(...)
  if (is.null(port)) {
    shinyApp(
      ui = app_ui(),
      server = app_server
    )
  } else {
    port <- try(as.integer(port))
    if (is.integer(port) && port > 0L) {
      shinyApp(
        ui = app_ui(),
        server = app_server,
        options = list(port = port)
      )
    }
  }
}
