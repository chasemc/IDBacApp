
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
  p("Note: For data integrity, samples cannot be removed from experiments.", align = "center"),
  p("Move strains between boxes by clicking the strain's name
  and then an arrow. Strains in the right box will be used for analysis."),
  IDBacApp::sampleChooserUI(ns("chooseNewDBSamples")),
  verbatimTextOutput(ns("selection")),
  br(),
  textInput(ns("nameformixNmatch"),
            label = "Enter name for new experiment"),
  
  actionButton(ns("addtoNewDB"),
               label = "Add to new Experiment")
  )
  
}




transferToNewDB_server <- function(input,
                                   output,
                                   session,
                                   pool,
                                   workingDirectory){
  

  nj <- shiny::callModule(IDBacApp::sampleChooser,
                          "chooseNewDBSamples",
                          pool = pool,
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
    
    newdbPath <- file.path(workingDirectory, paste0(input$nameformixNmatch, ".sqlite"))
    copyToNewDatabase(existingDBPool = databasePools$userDBcon,
                      newdbPath = newdbPath, 
                      sampleIDs = input$addSampleChooser$right)
    
    
    removeModal()
    
  })

}