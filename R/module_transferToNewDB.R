
#' transferToNewDB
#'
#' @param id id
#'
#' @return mod ui
#' @export
#'
transferToNewDB_UI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    
    div(align = "center",  
        h3("Transfer samples from previous experiments to new/other experiments."),
        tags$hr(size = 20),
        
        IDBacApp::databaseSelector_UI(ns("dbselector")),   
        tags$hr(size = 20),
        textInput(ns("newDBPath"),
                  label = "New experiment name:",
                  width = "50%",
                  placeholder = "Enter new experiment name here"),
        tags$hr(size = 20),
        p("Move samples between boxes by clicking the strain's name
  and then an arrow. Samples in the right box will be used for analysis."),
        IDBacApp::sampleChooser_UI(ns("chooseNewDBSamples")),
        tags$hr(size = 20),
        actionButton(ns("addtoNewDB"),
                     label = "Add to new Experiment")
    )
  )
  
}




#' transferToNewDB_server
#'
#' @param input .
#' @param output .
#' @param session .
#' @param sqlDirectory .
#' @param availableExperiments .
#'
#' @return .
#' @export
#'

transferToNewDB_server <- function(input,
                                   output,
                                   session,
                                   sqlDirectory,
                                   availableExperiments){
  
  
  selectedDB <-  shiny::callModule(IDBacApp::databaseSelector_server,
                                   "dbselector",
                                   availableExperiments = availableExperiments,
                                   sqlDirectory = sqlDirectory,
                                   h3Label = "Select an existing experiment to copy samples from:")
  
  
  
  chosenSamples <-  shiny::callModule(IDBacApp::sampleChooser_server,
                                      "chooseNewDBSamples",
                                      pool = selectedDB$userDBCon,
                                      allSamples = TRUE,
                                      whetherProtein = FALSE)
  
  
  
  shiny::callModule(IDBacApp::dbExists_server,
                    "dbExistsPopup")
  
  observeEvent(input$addtoNewDB, {
    # Number of samples for transferring must be > 0
    req(length(chosenSamples$chosen) > 0)
    
    newdbName <- gsub(" ",
                "",
                IDBacApp::path_sanitize(input$newDBPath))
    
    req(newdbName != "")
    
    IDBacApp::copyingDbPopup()
    
    dbExist <- file.exists(file.path(sqlDirectory$sqlDirectory, newdbName))
    
    if(dbExist){
    continue <- IDBacApp::dbExists_UI("dbExistsPopup", 
                            newdbName)
    } else {
      continue <- TRUE
    }
    if(continue == TRUE){
    
    
    IDBacApp::copyToNewDatabase(existingDBPool = selectedDB$userDBCon(),
                                newdbPath = sqlDirectory$sqlDirectory, 
                                newdbName = newdbName,
                                sampleIDs = chosenSamples$chosen)
    
   
    
    availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                                    pattern = ".sqlite",
                                                                    full.names = FALSE))
    }
    removeModal()
  })
  
}