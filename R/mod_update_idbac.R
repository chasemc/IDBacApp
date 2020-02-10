# Module UI
  
#' @title   mod_update_idbac_ui and mod_update_idbac_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_update_idbac
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_update_idbac_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_update_idbac
#' @export
#' @keywords internal
    
mod_update_idbac_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_update_idbac_ui("update_idbac_ui_1")
    
## To be copied in the server
# callModule(mod_update_idbac_server, "update_idbac_ui_1")
 
