#' News/Update module
#'
#' @return NA
#' @export
#'
newUpdate <- function(){
  
  showModal(
    modalDialog(
      size = "m",
      title = paste0("IDBac Version ", 
                     utils::packageVersion("IDBacApp")),
      shiny::includeMarkdown(system.file("NEWS.md", package = "IDBacApp")),
      easyClose = TRUE, 
      footer = "")
  )
  
}