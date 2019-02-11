selectInjections_UI <- function(id){
  ns <- NS(id)
  shiny::absolutePanel(
   # class = "dendMod_WellPanel",
    bottom = "50%",
    right =  "0%",
    width = "20%",
    fixed = T,
    draggable = TRUE,
    style = "z-index:1002;",
    shiny::wellPanel(class = "dendDots_WellPanel",
                     IDBacApp::databaseSelector_UI(ns("databaseSelector")),   
                     IDBacApp::sampleChooser_UI(ns("chooseNewDBSamples"))
                     
                     
                     

    )
    )
                     
                     
}
  
  
  
  selectInjections_server <- function(input,
                                      output,
                                      session,
                                      sqlDirectory,
                                      availableExperiments){
    
    
    
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
    
  }
    