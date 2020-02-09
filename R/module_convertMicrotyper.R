#' beginWithTXT
#'
#' @param id NA
#' 
#' @return NA
#' 
#'

convertMicrotyper_UI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Starting with Microtyper Data:"),
    p("Each input file is a \".txt\" file exported from Microtyper."),
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
#' @param availableExperiments availableExperiments
#'
#' @return .
#' 
#'

convertMicrotyper_Server <- function(input,
                                     output,
                                     session,
                                     tempMZDir,
                                     sqlDirectory,
                                     availableExperiments){
  
  
  # Reactive variable returning the user-chosen location of the raw delim files as string
  #----
  delimitedLocationP <- reactive({
    if (input$delimitedDirectoryP > 0) {
      choose_dir()
    }
  })
  
  
  # Reactive variable returning the user-chosen location of the raw delim files as string
  #----
  delimitedLocationSM <- reactive({
    if (input$delimitedDirectorySM > 0) {
      choose_dir()
    }
  })
  
  
  
  output$newExperimentNameText <- renderText({
    a <- gsub(" ", "", sanitize(input$newExperimentName))
    
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
    shiny::validate(shiny::need(!is.null(delimitedLocationSM()),
                                "No small molecule directory selected."))
    names <- tools::file_path_sans_ext(base::basename(smallMolFiles()))
    names <- paste0(names, collapse = " \n ")
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$delimitedLocationPo <- renderText({
    shiny::validate(shiny::need(!is.null(delimitedLocationP()),
                                "No protein directory selected."))
    names <- tools::file_path_sans_ext(base::basename(proteinFiles()))
    paste0(names, collapse = " \n ")
    
  })
  
  proteinFiles <- reactive({
    getMicrotyperFiles(delimitedLocationP())
  })
  
  smallMolFiles <- reactive({
    getMicrotyperFiles(delimitedLocationSM())
  })
  
  
  
  
  sanity <- reactive({
    a <- sanitize(input$newExperimentName)
    gsub(" ",
         "",
         a)
  })
  
  
  
  # Run raw data processing on delimited-type input files
  #----
  observeEvent(input$runDelim, 
               ignoreInit = TRUE, {
                 
                 # shiny::validate(shiny::need(!is.null(sanity()), 
                 #                             "Filename must not be empty"))
                 # 
                 # shiny::validate(shiny::need(sanity() != "",
                 #                             "Filename must not be empty"))
                 # 
                 # shiny::validate(shiny::need(is.null(proteinFiles()) + is.null(smallMolFiles()) > 0,
                 #                             "No samples selected to process"))
                 # 
                 # shiny::validate(shiny::need(length(proteinFiles()) + length(smallMolFiles()) > 0,
                 #                             "No samples selected to process"))
                 
                 popup3()
                 
                 keys <- run_microtyperTomzML(proteinPaths = proteinFiles(),
                                                        smallMolPaths = smallMolFiles(),
                                                        exportDirectory = tempMZDir)
                 
                 idbac_create(fileName = input$newExperimentName,
                              filePath = sqlDirectory$sqlDirectory)
                 
                 idbacPool <- idbac_connect(fileName = input$newExperimentName,
                                            filePath = sqlDirectory$sqlDirectory)[[1]]
                 
                 process_mzml(mzFilePaths = keys$mzFilePaths,
                                        sampleIds = keys$sampleID,
                                        idbacPool = idbacPool,
                                        acquisitionInfo = NULL)
                 pool::poolClose(idbacPool)
                 
                 
                 popup4()
                 # Update available experiments
                 availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                                                 pattern = ".sqlite",
                                                                                 full.names = FALSE))
               })
  
  
  
  
  
} 




