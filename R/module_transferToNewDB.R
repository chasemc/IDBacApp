
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
    
    
    h3("Transfer samples from previous experiments to new/other experiments.", align = "center"),
    p("Move strains between boxes by clicking the strain's name
  and then an arrow. Strains in the right box will be used for analysis."),
    IDBacApp::databaseSelector_UI(ns("dbselector")),   
    verbatimTextOutput(ns("selection")),
    br(),
    textInput(ns("nameformixNmatch"),
              label = "New experiment name:",
              width = "50%",
              placeholder = "Enter new experiment name here"),
    IDBacApp::sampleChooser_UI(ns("chooseNewDBSamples")),
    actionButton(ns("addtoNewDB"),
                 label = "Add to new Experiment"),
  
  p("Note: For data integrity, samples cannot be removed from experiments.", 
    align = "center",
    class = "note")
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
                                   sqlDirectory = sqlDirectory)
  
  
  
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
    copyingDbPopup()
    
    newdbPath <- file.path(sqlDirectory, paste0(input$nameformixNmatch, ".sqlite"))
    copyToNewDatabase(existingDBPool = selectedDB$userDBCon(),
                      newdbPath = newdbPath, 
                      sampleIDs = chosenSamples$addSampleChooser$right)
    
    
    removeModal()
    availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory,
                                                                    pattern = ".sqlite",
                                                                    full.names = FALSE))
  })
  
}