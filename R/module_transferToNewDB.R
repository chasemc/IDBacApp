
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
        verbatimTextOutput(ns("selection")),
        tags$hr(size = 20),
        textInput(ns("nameformixNmatch"),
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
  
  
  
  copyingDbPopup <- reactive({
    showModal(modalDialog(
      title = "Important message",
      "When file-conversions are complete this pop-up will be replaced by a summary of the conversion.",
      br(),
      "To check what has been converted, you can navigate to:",
      easyClose = FALSE, 
      size = "l",
      footer = ""))
  })
  
  
  
  
  
  
  observeEvent(input$addtoNewDB, {
    
    nam <- gsub(" ", "", IDBacApp::path_sanitize(input$nameformixNmatch))
    
    req(nam != "")
    
    copyingDbPopup()
    
    newdbPath <- file.path(sqlDirectory$sqlDirectory, paste0(nam, ".sqlite"))
    copyToNewDatabase(existingDBPool = selectedDB$userDBCon(),
                      newdbPath = newdbPath, 
                      sampleIDs = chosenSamples$chosen)
    
    removeModal()
    availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                                    pattern = ".sqlite",
                                                                    full.names = FALSE))
  })
  
}