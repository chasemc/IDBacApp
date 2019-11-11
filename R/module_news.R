#' News/Update module
#'
#' @return NA
#' @export
#'
newUpdate <- function(){
  if (isTRUE(.Options$idbac_testing)) {
  showModal(
    modalDialog(
      size = "m",
      title = paste0("IDBac Version ", 
                     utils::packageVersion("IDBacApp")),
      shiny::includeMarkdown(system.file("NEWS.md", package = "IDBacApp")),
      easyClose = TRUE, 
      footer = modalButton("Dismiss"))
  )
  }
}