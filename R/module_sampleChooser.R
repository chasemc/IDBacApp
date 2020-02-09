
#' UI for choosing all samples fromm an IDBac DB
#'
#' @param id namespace
#'
#' @return UI
#' 
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
#' @return NA
#' 
#'
sampleChooser_server <- function(input,
                                 output,
                                 session,
                                 pool,
                                 whetherProtein = FALSE,
                                 allSamples = FALSE){
  
  chosenProteinSampleIDs <- reactiveValues(chosen = NULL)
  nams <- reactiveValues(av = NULL)
  
  observe({
    chosenProteinSampleIDs$chosen <- NULL
    conn <- pool::poolCheckout(pool())
    nams$av <- idbac_available_samples(pool = conn,
                                       whetherProtein = whetherProtein,
                                       allSamples = allSamples)
    
    pool::poolReturn(conn)
    
  })
  
  observeEvent(input$addSampleChooser, ignoreInit = TRUE, {
    chosenProteinSampleIDs$chosen <- input$addSampleChooser$right 
  })
  
  
  
  output$chooseSamples <- renderUI({
    
    tagList(chooserInput(inputId = session$ns("addSampleChooser"),
                         leftLabel = "Available samples",
                         rightLabel = "Selected samples",
                         leftChoices = nams$av,
                         rightChoices = c(),
                         size = 10,
                         multiple = TRUE)
    )
    
  })
  
  
  
  
  
  return(chosenProteinSampleIDs)
}