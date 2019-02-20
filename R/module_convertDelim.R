#' beginWithTXT
#'
#' @param id NA
#'
#' @return NA
#' @export
#'

convertDelim_UI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Starting with tab, csv, or txt files:"),
    p("Each file should only contain a single profile spectrum formatted in two columns,", 
      br(),
      "the first being ", tags$i("m/z")," and the second column being intensity values"),
    br(),
    p(strong("1: Enter a name for this new experiment")),
    p("This will become a filename, non-valid characters will be removed."),
    p("Hint: Intead of a space, use \"_\"."),
    textInput(ns("newExperimentName"),
              label = "",
              width = "50%",
              placeholder = "Enter Experiment Name Here"),
    verbatimTextOutput(ns("newExperimentNameText"),
                       placeholder = TRUE),
    tags$hr(size = 20),
    p(strong("2: Click to select the location of your files"),
      align = "center"),
    p("At least one protein or one small molecule file is required."),
    radioButtons(ns("centroid"),
                 "Data Type",
                 choices = list("Currently IDBac only accepts profile data" = FALSE),
                 selected = FALSE),
    actionButton(ns("delimitedDirectoryP"),
                 label = "Click to select folder containing protein data"),
    
    verbatimTextOutput(ns("delimitedLocationPo"),
                       placeholder = FALSE),
    
    actionButton(ns("delimitedDirectorySM"),
                 label = "Click to select folder containing small-molecule data"),
    
    verbatimTextOutput(ns("delimitedLocationSMo"),
                       placeholder = FALSE),
    tags$hr(size = 20),
    p(strong("3: Begin conversion")),
    
    actionButton(ns("runDelim"),
                 label = "Process Data")
    
  )
}






#' convertDelim_Server
#'
#' @param input module
#' @param output module
#' @param session module
#' @param tempMZDir tempMZDir 
#' @param sqlDirectory sqlDirectory 
#'
#' @return .
#' @export
#'

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
  
  
  
  output$newExperimentNameText <- renderText({
    a <- gsub(" ", "", IDBacApp::path_sanitize(input$newExperimentName))
    
    if (a == "") {
      "Once entered, the filename-friendly version of the entered name will appear here once. \n
      This will be the version of your experiment name that is saved."
    } else {
      a
    }
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$delimitedLocationSMo <- renderText({
    req(smallMolFiles())
    names <- tools::file_path_sans_ext(base::basename(smallMolFiles()))
    names <- paste0(names, collapse = " \n ")
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$delimitedLocationPo <- renderText({
    req(proteinFiles())
    names <- tools::file_path_sans_ext(base::basename(proteinFiles()))
    names <- paste0(names, collapse = " \n ")
    
  })
  
  proteinFiles <- reactive({
    
    if (is.null(delimitedLocationP())) {
      NULL
    } else {
      
      foldersInFolder <- list.files(delimitedLocationP(), 
                                    recursive = TRUE, 
                                    full.names = TRUE,
                                    pattern = "(.txt|.tab|.csv)$") # Get the folders contained directly within the chosen folder.
    }
  })
  
  
  
  smallMolFiles <- reactive({
    
    if (is.null(delimitedLocationSM())) {
      NULL
    } else {    
      foldersInFolder <- list.files(delimitedLocationSM(), 
                                    recursive = TRUE, 
                                    full.names = TRUE,
                                    pattern = "(.txt|.tab|.csv)$") # Get the folders contained directly within the chosen folder.
    }
  })
  
  
  
  
  sanity <- reactive({
    a <- IDBacApp::path_sanitize(input$newExperimentName)
    gsub(" ","",a)
  })
  
  
  
  
  # Run raw data processing on delimited-type input files
  #----
  observeEvent(input$runDelim, 
               ignoreInit = TRUE, {
                 req(!is.null(sanity()))
                 req(sanity() != "")
                 
              
                 req(is.null(proteinFiles()) + is.null(smallMolFiles()) > 0)
                 req(length(proteinFiles()) + length(smallMolFiles()) > 0)
                 
                 
                 
                 
                 if (is.null(smallMolFiles())) {
                   smallPaths <- NULL
                   sampleNameSM <- NULL
                 } else {
                   smallPaths <- smallMolFiles()
                   sampleNameSM <- tools::file_path_sans_ext(basename(smallPaths))
                   sampleNameSM <- unlist(lapply(sampleNameSM, function(x) strsplit(x, "-")[[1]][[1]]))
                   
                 }
                 
                 if (is.null(proteinFiles())) {
                   proteinPaths <- NULL
                   sampleNameP <- NULL
                 } else {
                   proteinPaths <- proteinFiles()
                   sampleNameP <- tools::file_path_sans_ext(basename(proteinPaths))
                   sampleNameP <- unlist(lapply(sampleNameP, function(x) strsplit(x, "-")[[1]][[1]]))
                 }
                 
                 
                 
                 
                 keys <- IDBacApp::parseDelimitedMS(proteinPaths = proteinPaths,
                                                    proteinNames = sampleNameP,
                                                    smallMolPaths = smallPaths,
                                                    smallMolNames = sampleNameSM,
                                                    exportDirectory = tempMZDir,
                                                    centroid = input$centroid)
                 
                 IDBacApp::processMZML(mzFilePaths = keys$mzFilePaths,
                                       sampleIds = keys$sampleIds,
                                       sqlDirectory = sqlDirectory$sqlDirectory,
                                       newExperimentName = input$newExperimentName)
                 
                 
                 
                 IDBacApp::popup4()
               })
  
  
  
} 




