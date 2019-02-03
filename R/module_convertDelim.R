#' beginWithTXT
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
convertDelim_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    p(".txt and .csv support coming soon!"),
    textInput(ns("newExperimentName"),
              label = "New Experiment Name",
              width = "50%"),
    radioButtons(ns("centroid"),
                 "Data Type",
                 choices = list("Currently IDBac only accepts profile data" = FALSE),
                 selected = FALSE),
    actionButton(ns("delimitedDirectoryP"),
                 label = "Raw Data P Folder"),
    actionButton(ns("delimitedDirectorySM"),
                 label = "Raw Data SM Folder"),
    actionButton(ns("runDelim"),
                 label = "Process Data"),
    verbatimTextOutput(ns("delimitedLocationPo"),
                       placeholder = TRUE),
    verbatimTextOutput(ns("delimitedLocationSMo"),
                       placeholder = TRUE)
  )
}






convertDelim_Server <- function(input,
                                output,
                                session,
                                tempMZDir,
                                sqlDirectory){
  
  
  # Reactive variable returning the user-chosen location of the raw delim files as string
  #----
  delimitedLocationP <- reactive({
    if (input$delimitedDirectoryP > 0) {
      IDBacApp::choose_dir()
    }
  })
  
  
  # Reactive variable returning the user-chosen location of the raw delim files as string
  #----
  delimitedLocationSM <- reactive({
    if (input$delimitedDirectorySM > 0) {
      IDBacApp::choose_dir()
    }
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$delimitedLocationSMo <- renderText({
    if (is.null(delimitedLocationSM())) {
      return("No Folder Selected")} else {
        folders <- NULL
        foldersInFolder <- list.files(delimitedLocationSM(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
        for (i in 1:length(foldersInFolder)) {
          folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
        }
        folders
      }
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$delimitedLocationPo <- renderText({
    if (is.null(delimitedLocationP())) {
      return("No Folder Selected")} else {
        folders <- NULL
        foldersInFolder <- list.files(delimitedLocationP(), recursive = FALSE, full.names = FALSE) # Get the folders contained directly within the chosen folder.
        for (i in 1:length(foldersInFolder)) {
          folders <- paste0(folders, "\n", foldersInFolder[[i]]) # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
        }
        folders
      }
  })
  
  
  
  
  # Run raw data processing on delimited-type input files
  #----
  observeEvent(input$runDelim, 
               ignoreInit = TRUE, {
                 
                 IDBacApp::popup3()
                 
                 keys <- IDBacApp::parseDelimitedMS(sampleNames = NULL,
                                                    proteinDirectory = delimitedLocationP(),
                                                    smallMolDirectory = delimitedLocationSM(),
                                                    exportDirectory =  sqlDirectory,
                                                    centroid = input$centroid)
                 
                 
                 
                 IDBacApp::processMZML(mzFilePaths = normalizePath(file.path(sqlDirectory,
                                                                             paste0(keys,
                                                                                    ".mzML"))) ,
                                       sampleIds = keys,
                                       sqlDirectory = sqlDirectory,
                                       newExperimentName = input$newExperimentName)
                 
                 
                 
                 IDBacApp::popup4()
               })
  
  
  
} 