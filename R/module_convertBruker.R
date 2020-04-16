
#' oneMaldiPlate
#'
#' @param id NA
#'
#' @return NA
#' 
#'

convertOneBruker_UI <- function(id){
  ns <- NS(id)
  tagList( 
    h3("Starting with a single MALDI plate of Bruker data", align = "center"),
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
    p(strong("2: Click to select your folder that contains data from a single MALDI plate."), align = "center"),
    actionButton(ns("rawFileDirectory"),
                 label = "Raw Data Folder"),
    verbatimTextOutput(ns("rawFileDirectoryText"),
                       placeholder = TRUE),
    tags$hr(size = 20),
    p(strong("3:", "Fill in the Sample-ID spreadsheet.")),
    
    actionButton(ns("showSampleMap"), "Click to Open Spreadsheet"),
    tags$hr(size = 20),
    p(strong("You must label all spots that contain data."), br(),
      strong("IDBac found data for the following spots that aren't labelled in the spreadsheet:")
    ),
    shiny::verbatimTextOutput(ns("missingSampleNames"), placeholder = TRUE),
    p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
    actionButton(ns("convertSingleBruker"),
                 label = "Process Data"),
    tags$hr(size = 20)
  )
  
}





#' convertOneBruker_Server
#'
#' @param input NS
#' @param output NS
#' @param session NS
#' @param tempMZDir tempMZDir 
#' @param sqlDirectory sqlDirectory
#' @param availableExperiments availableExperiments
#'
#' @return NA
#'
convertOneBruker_Server <- function(input,
                                    output,
                                    session,
                                    tempMZDir,
                                    sqlDirectory,
                                    availableExperiments){
  
  
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  rawFilesLocation <- reactive({
    
    req(input$rawFileDirectory > 0)
    loc <- choose_dir()
    if (!is.na(loc)) {
      return(loc)
    }
    
  })
  
  
  sanitizedNewExperimentName <- reactive({
    
    sanitize(input$newExperimentName)
    
  })
  
  
  output$newExperimentNameText <- renderText({
    
    sanitized <- sanitizedNewExperimentName()
    
    if (sanitized == "") {
      return("The filename-friendly version of your entry will appear here.")
    } else {
      return(sanitized)
    }
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$rawFileDirectoryText <- renderText({
    
    if (is.null(rawFilesLocation())) {
      
      return("No Folder Selected")
      
    } else {
      
      folders <- NULL
      # Get the folders contained within the chosen folder.
      foldersInFolder <- list.dirs(rawFilesLocation(),
                                   recursive = FALSE,
                                   full.names = FALSE) 
      for (i in 1:length(foldersInFolder)) {
        # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
        folders <- paste0(folders, 
                          "\n",
                          foldersInFolder[[i]])
      }
      return(folders)
    }
  })
  
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  multipleMaldiRawFileLocation <- reactive({
    req(input$multipleMaldiRawFileDirectory > 0)
    choose_dir()
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$multipleMaldiRawFileDirectory <- renderText({
    if (is.null(multipleMaldiRawFileLocation())) {
      return("No Folder Selected")
    } else {
      folders <- NULL
      # Get the folders contained within the chosen folder.
      foldersInFolder <- list.dirs(multipleMaldiRawFileLocation(),
                                   recursive = FALSE, 
                                   full.names = FALSE) 
      for (i in 1:length(foldersInFolder)) {
        # Creates user feedback about which raw data folders were chosen. 
        # Individual folders displayed on a new line "\n"
        folders <- paste0(folders, "\n", foldersInFolder[[i]]) 
      }
      
      return(folders)
    }
  })
  
  
  # list of acqus info
  acquisitonInformation <- reactive({
    
    readBrukerAcqus(rawFilesLocation())
    
  })
  
  
  
  # Sample Map --------------------------------------------------------------
  
  anyMissing <- reactive({
    
    req(rawFilesLocation())
    req(sampleMapReactive$rt)
    spots <- unlist(lapply(acquisitonInformation(), function(x) x$spot))
    findMissingSampleMapIds(spots = spots, 
                            sampleMap = sampleMapReactive$rt,
                            ignoreMissing = TRUE)
  })
  
  
  output$missingSampleNames <- shiny::renderText({
    
    if (length(anyMissing()$missing) == 0) {
      paste0("No missing IDs")
    } else {
      paste0(paste0(anyMissing()$missing, collapse = " \n ", sep = ","))
    }
  })
  
  sampleMapReactive <- reactiveValues(rt = nulledMap384Well())
  
  observeEvent(input$showSampleMap, 
               ignoreInit = TRUE, {  
                 ns <- session$ns
                 showModal(modalDialog(footer = actionButton(ns("saveSampleMap"), "Save/Close"),{
                   tagList(
                     HTML("This spreadsheet represents a MALDI plate and will work with plate sizes of 384-spots or less. <br>
                          Enter sample names as they were arranged on your MALDI plate. <br>
                          You can also copy/paste from Excel."),
                     rhandsontable::rHandsontableOutput(ns("plateDefault"))
                     
                   )
                 }))
                 
                 
                 
               })
  
  
  output$plateDefault <- rhandsontable::renderRHandsontable({
    
    sampleMapViewer(sampleMapReactive$rt)
    
  })
  
  
  observeEvent(input$saveSampleMap, 
               ignoreInit = TRUE, {
                 sampleMapReactive$rt <- sampleMaptoDF(sampleData = input$plateDefault)
                 
                 shiny::removeModal()
               })
  
  
  
  
  
  
  success <- reactiveValues(val = FALSE)
  
  # Call the Spectra processing function when the spectra processing button is pressed
  #----
  observeEvent(input$convertSingleBruker,
               ignoreInit = TRUE,  {
                 
                 req(length(anyMissing()$missing) == 0)
                 req(!is.null(sanitizedNewExperimentName()))
                 req(sanitizedNewExperimentName() != "")
                 validate(need(any(!is.na(sampleMapReactive$rt)), 
                               "No samples entered into sample map, please try entering them again"))
                 
                 # # get target positions in order of acqus list
                 # spots <- unlist(lapply(acquisitonInformation(), function(x){
                 #   x$spot
                 # }))
                 # 
                 # 
                 # # should be the same because both come from acquisitonInformation()
                 # validate(need(identical(sort(names(anyMissing()$matching)), 
                 #                         sort(unique(spots))),
                 #               list("Something happend when associating Bruker acqu spots", "\n",
                 #                    "names:", sort(names(anyMissing()$matching)), "\n",
                 #                    "spots:", sort(unique(spots)))
                 # ))
                 
                 db_from_bruker(dataDirectory = input$rawFileDirectory,
                                fileName = sanitizedNewExperimentName(),
                                filePath = sqlDirectory$sqlDirectory,
                                anyMissing = anyMissing(),
                                acquisitionInfo = acquisitonInformation(),
                                sampleMap  = NULL,
                                tempDir = tempMZDir)
                 
                 # Update available experiments
                 availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                                                 pattern = ".sqlite",
                                                                                 full.names = FALSE))
                 popup4()
                 
               })
  
}





#'  Rhandsontable Sample Map 
#'
#'  Rhandsontable 
#'  
#' @param df reactiveValues data frame input nulledMap384Well()
#'
#' @return rhandsontable
#' 
#'
sampleMapViewer <- function(df){
  rhandsontable::rhandsontable(df,
                               useTypes = FALSE,
                               contextMenu = TRUE,
                               maxRows = 16,
                               maxRows = 24) %>%
    rhandsontable::hot_context_menu(allowRowEdit = FALSE,
                                    allowColEdit = FALSE) %>%
    rhandsontable::hot_cols(colWidths = 100) %>%
    rhandsontable::hot_rows(rowHeights = 25) 
}



#' Update sample map reactive value
#'
#' @param sampleData sample map rhandsontable
#'
#' @return none, side effect
#' 
#'
sampleMaptoDF <- function(sampleData){
  as.data.frame(rhandsontable::hot_to_r(sampleData), 
                stringsAsFactors = FALSE)
}