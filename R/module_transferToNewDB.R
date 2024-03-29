
#' transferToNewDB
#'
#' @param id id
#'
#' @return mod ui
#' 
#'
transferToNewDB_UI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    
    div(align = "center",  
        h3("Transfer samples from previous experiments to new/other experiments."),
        tags$hr(size = 20),
        
        databaseSelector_UI(ns("dbselector")),   
        tags$hr(size = 20),
        textInput(ns("newDBPath"),
                  label = "New experiment name:",
                  width = "50%",
                  placeholder = "Enter new experiment name here"),
        tags$hr(size = 20),
        p("Move samples between boxes by clicking the strain's name
  and then an arrow. Samples in the right box will be used for analysis."),
        sampleChooser_UI(ns("chooseNewDBSamples")),
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
#' 
#'

transferToNewDB_server <- function(input,
                                   output,
                                   session,
                                   sqlDirectory,
                                   availableExperiments){
  
  
  selectedDB <-  shiny::callModule(databaseSelector_server,
                                   "dbselector",
                                   availableExperiments = availableExperiments,
                                   sqlDirectory = sqlDirectory,
                                   h3Label = "Select an existing experiment to copy samples from:")
  
  
  
  chosenSamples <-  shiny::callModule(sampleChooser_server,
                                      "chooseNewDBSamples",
                                      pool = selectedDB$pool,
                                      type = "all")
  
  
  continue <- reactiveValues(val = FALSE)
  
  
  
  
  observeEvent(input$addtoNewDB,
               ignoreInit = TRUE, {
                 
                 # Number of samples for transferring must be > 0
                 req(length(chosenSamples$chosen) > 0)
                 
                 newDBName <- gsub(" ",
                                   "",
                                   sanitize(input$newDBPath))
                 
                 req(newDBName != "")
                 
                 
                 dbExist <- file.exists(file.path(sqlDirectory$sqlDirectory, paste0(newDBName,".sqlite")))
                 
                 if(dbExist){
                   ns <- session$ns
                   
                   showModal(
                     modalDialog(
                       title = "Warning",
                       glue::glue("Experiment: {newDBName} already exists, overwrite?"),
                       easyClose = FALSE, 
                       size = "m",
                       actionButton(ns("continueTransfer"), "Continue", icon = icon("check")),
                       actionButton(ns("stopTransfer"), "Stop", icon = icon("remove")),
                       footer = NULL
                     )
                   )
                 } else {
                   continue$val <- TRUE
                 }
                 
               })
  
  
  
  
  observeEvent(input$continueTransfer, 
               ignoreInit = TRUE, {
                 removeModal()
                 continue$val <- TRUE   
                 
               })
  
  
  
  
  
  observeEvent(input$stopTransfer, 
               ignoreInit = TRUE, {
                 continue$val <- FALSE    
                 removeModal()
                 
               })
  
  
  
  
  
  
  observeEvent(continue$val, 
               ignoreInit = TRUE, {
                 
                 req(continue$val == TRUE)
                 copyingDbPopup()
                 
                 newDBName <- gsub(" ",
                                   "",
                                   sanitize(input$newDBPath))
                 
                 req(newDBName != "")
                 
                 newCheckedPool <- idbac_connect(newDBName, 
                                                        sqlDirectory$sqlDirectory)
                 
                 shiny::withProgress(message = 'Copying data to new database',
                                     detail = 'Connecting experiments...',
                                     value = 0, {
                                       copyToNewDatabase(existingDBPool = selectedDB$pool(),
                                                                   newDBPool = newCheckedPool[[1]], 
                                                                   sampleIDs = chosenSamples$chosen)
                                     })
                 
                 removeModal()
                 
                 availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                                                 pattern = ".sqlite",
                                                                                 full.names = FALSE))
                 continue$val <- FALSE
                 selectedDB <- NULL
               })
  
  
  
  
  
}










