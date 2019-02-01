
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
    uiOutput(ns("chooseSamples"))
  )
}



#' Server for choosing all samples fromm an IDBac DB
#'
#' @param input  shiny module
#' @param output shiny module
#' @param session shiny module
#' @param pool reactive object that contains a pool to currently-chosen database
#' @param whetherProtein T  "allSamples takes precedence
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
  nams <- reactiveValues()

  observeEvent(selectedDB$selectExperiment, {
    req(class(pool())[[1]] == "Pool")
    conn <- pool::poolCheckout(pool())
    
    nams$av <- IDBacApp::availableSampleNames(checkedPool = conn,
                                              whetherProtein = whetherProtein,
                                              allSamples = allSamples)
    pool::poolReturn(conn)
    
    
  })
  
  
  
  output$chooseSamples <- renderUI({
    
     tagList(IDBacApp::chooserInput(inputId = session$ns("addSampleChooser"),
                                   leftLabel = "Available samples",
                                   rightLabel = "Selected samples",
                                   leftChoices = nams$av,
                                   rightChoices = c(),
                                   size = 10,
                                   multiple = TRUE)
    )
    
  })
  
  
  
  
  
  return(input)
}