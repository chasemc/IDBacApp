
#' UI for choosing all samples fromm an IDBac DB
#'
#' @param id namespace
#'
#' @return UI
#' @export
#'
sampleChooser_UI <- function(id) {
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
sampleChooser_server <- function(input,
                          output,
                          session,
                          pool,
                          whetherProtein = FALSE,
                          allSamples = FALSE,
                          selectedDB){
  
  
  amcd <- reactive({
    selectedDB$selectExperiment
    conn <- pool::poolCheckout(pool())
    print("poo")
    IDBacApp::availableSampleNames(checkedPool = conn,
                                   whetherProtein = whetherProtein,
                                   allSamples = allSamples)
    pool::poolReturn(conn)
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
  
  
  
  return(input)
}