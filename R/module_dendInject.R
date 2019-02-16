#' selectInjections_UI
#'
#' @param id namespace
#'
#' @return NA
#' @export
#'
selectInjections_UI <- function(id){
  ns <- NS(id)
  tagList(
  div(align = "center",
    actionButton(ns("openInject"), "Insert samples from another experiment")
  ),
  uiOutput(ns("injectSelection"))
  )
                     
}
  
  
  
#' Inject samples from another database into dendrogram
#'
#' @param input shiny
#' @param output shiny 
#' @param session shiny
#' @param sqlDirectory directory containing sqlite files
#' @param availableExperiments available sqlite files
#' @param watchMainDb for reacttivity
#'
#' @return NA
#' @export
#'
  selectInjections_server <- function(input,
                                      output,
                                      session,
                                      sqlDirectory,
                                      availableExperiments,
                                      watchMainDb){
    
    
    
    
    observeEvent(input$closeInject, {
      output$injectSelection <- renderUI({
        # Intentionally Blank
      })
      
      
    })  
    
    observeEvent(input$openInject, ignoreInit = T ,ignoreNULL = T, {
      ns <- session$ns
      
      output$injectSelection <- renderUI(
        shiny::absolutePanel(
          # class = "dendMod_WellPanel",
          bottom = "5%",
          right =  "5%",
          width = "50%",
          fixed = T,
          draggable = TRUE,
          style = "z-index:1002;",
          shiny::wellPanel(class = "dendDots_WellPanel",
                           IDBacApp::databaseSelector_UI(ns("databaseSelector")),   
                           IDBacApp::sampleChooser_UI(ns("chooseNewDBSamples")),
                           shiny::actionButton(ns("closeInject"),
                                               "Close")
                          
          )
        )
        
      )  
    })
    
    chosenSamples <- reactiveValues(chosen = NULL)
    
    
    observeEvent(watchMainDb$selectExperiment, {
      
      chosenSamples$chosen <- NULL  
      
    })
    
    
    
    
    selectedDB <- callModule(IDBacApp::databaseSelector_server,
                             "databaseSelector",
                             h3Label = tags$h4("Select an experiment ", br(), "to work with:"),
                             availableExperiments = availableExperiments,
                             sqlDirectory = sqlDirectory)
    
    chosen <-  shiny::callModule(IDBacApp::sampleChooser_server,
                                 "chooseNewDBSamples",
                                 pool = selectedDB$userDBCon,
                                 allSamples = TRUE,
                                 whetherProtein = FALSE)
  observeEvent(chosen$chosen, {  
    chosenSamples$chosen <- chosen$chosen
    
  })
    
    return(list(chosen = chosenSamples,
                db = selectedDB$userDBCon))
  }
    