

#' plateMapUI
#'
#' @param id namespace
#'
#' @return UI
#' @export
#'

plateMapUI <- function(id) {
  ns <- shiny::NS(id)
  
  showModal(modalDialog({
    tagList(
    rHandsontableOutput(ns("plateDefault")),
    actionButton(ns("save"), "Save")
    )
  }))
  
  
  
}




#' platemod
#'
#' @param input shiny module
#' @param output shiny module
#' @param session shiny module
#'
#' @return rhandsontable 
#' @export
#'

platemod <- function(input,
                     output,
                     session){
  
  
  
  qwerty <- reactiveValues(rtab = NULL)
  
  
  output$plateDefault <- rhandsontable::renderRHandsontable({
    
    if(is.null(qwerty$rtab)){
      qwerty$rtab <- as.data.frame(base::matrix(NA,
                                 nrow = 16,
                                 ncol = 24,
                                 dimnames = list(LETTERS[1:16],1:24)))
    }
    
   rhandsontable::rhandsontable(qwerty$rtab,
                                 useTypes = FALSE,
                                 contextMenu = TRUE ) %>%
      hot_context_menu(allowRowEdit = FALSE,
                       allowColEdit = TRUE) %>%
      hot_cols(colWidths = 100) %>%
      hot_rows(rowHeights = 25)
  })
  

 observe(print(qwerty$rtab))


 observeEvent(input$save, {
   aq <- rhandsontable::hot_to_r(input$plateDefault)
  })
      
return(reactive(qwerty$rtab))
    
    
  }
    
  
