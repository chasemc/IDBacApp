
#' UI for choosing all samples fromm an IDBac DB
#'
#' @param id namespace
#'
#' @return UI
#' @export
#'
sampleChooserUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::uiOutput(ns("chooseSamples"))
    
  )
}



#' Server for choosing all samples fromm an IDBac DB
#'
#' @param input  NA
#' @param output NA
#' @param session NA
#' @param pool pool
#' @param whetherProtein T/F 
#' @param allSamples T/F
#'
#' @return 
#' @export
#'
sampleChooser <- function(input,
                          output,
                          session,
                          pool,
                          whetherProtein = FALSE,
                          allSamples = FALSE){
  
  
amcd <- reactive({
  IDBacApp::availableSampleNames(pool = pool(),
                                 whetherProtein = whetherProtein,
                                 allSamples = allSamples)
})  

    output$chooseSamples <- renderUI({
      ns <- session$ns
 
    IDBacApp::chooserInput(ns("addSampleChooser"),
                           "Available samples",
                           "Selected samples",
                           amcd(),
                           c(),
                           size = 10,
                           multiple = TRUE
    )
  })
  
  

  return(input$addSampleChooser)
}