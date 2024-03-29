#' selectDirectory_UI
#'
#' @param id namespace
#' @param label label of the directory action button
#'
#' @return NA
#' 
#'
selectDirectory_UI <- function(id, label){
  
  ns <- NS(id)
  
  actionButton(ns("rawFileDirectory"),
               label = label)
  
  
}


#' selectDirectory_Server
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param sqlDirectory sqlDirectory
#'
#' @return NA
#' 
#'

selectDirectory_Server <- function(input,
                                   output,
                                   session,
                                   sqlDirectory){
  
  
  observeEvent(input$rawFileDirectory, {
    loc <- choose_dir()
    
    if (!is.na(loc)) {
      sqlDirectory$sqlDirectory <- loc
    } else {
      sqlDirectory$sqlDirectory <- sqlDirectory$sqlDirectory
    }
    
    
  })
  
  
}

#----

