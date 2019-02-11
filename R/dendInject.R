selectInjections_UI <- function(id){
  ns <- NS(id)
  tagList(
  actionButton(ns("openInject"), "Inject Samples"),
  uiOutput(ns("injectSelection"))
  )
                     
}
  
  
  
  selectInjections_server <- function(input,
                                      output,
                                      session,
                                      sqlDirectory,
                                      availableExperiments){
    
    
    
    
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    selectedDB <- callModule(IDBacApp::databaseSelector_server,
                             "databaseSelector",
                             h3Label = tags$h4("Select an experiment ", br(), "to work with:"),
                             availableExperiments = availableExperiments,
                             sqlDirectory = sqlDirectory)
    
    chosenSamples <-  shiny::callModule(IDBacApp::sampleChooser_server,
                                        "chooseNewDBSamples",
                                        pool = selectedDB$userDBCon,
                                        allSamples = TRUE,
                                        whetherProtein = FALSE)
    return(list(chosen = chosenSamples,
                db = selectedDB$userDBCon))
  }
    