
#' oneMaldiPlate
#'
#' @param id NA
#'
#' @return NA
#' @export
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
    if (input$rawFileDirectory > 0) {
      loc <- IDBacApp::choose_dir()
      
      if (!is.na(loc)) {
        return(loc)
      }
    }
  })
  
  
  output$newExperimentNameText <- renderText({
    a <- gsub(" ", "", IDBacApp::path_sanitize(input$newExperimentName))
    
    if (a == "") {
      "The filename-friendly version of your entry will appear here."
    } else {
      a
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
    if (input$multipleMaldiRawFileDirectory > 0) {
      IDBacApp::choose_dir()
    }
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
  
  
  # Sample Map --------------------------------------------------------------
  
  anyMissing <- reactive({
    req(rawFilesLocation())
    req(sampleMapReactive$rt)
    
    
    #######TODO TURN rawFilesLocation() INTO SPOTS
    findMissingSampleMapIds(spots = , 
                            sampleMap = sampleMapReactive$rt)
    
  })
  
  
  output$missingSampleNames <- shiny::renderText({
    
    
    if (length(anyMissing()) == 0) {
      paste0("No missing IDs")
    } else {
      paste0(paste0(anyMissing(), collapse = " \n ", sep = ","))
    }
  })
  
  sampleMapReactive <- reactiveValues(rt = as.data.frame(base::matrix(NA,
                                                                      nrow = 16,
                                                                      ncol = 24,
                                                                      dimnames = list(LETTERS[1:16],1:24))))
  
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
  
  
  observeEvent(input$saveSampleMap, 
               ignoreInit = TRUE, {  
                 
                 shiny::removeModal()
                 
               })
  
  
  
  output$plateDefault <- rhandsontable::renderRHandsontable({
    
    rhandsontable::rhandsontable(sampleMapReactive$rt,
                                 useTypes = FALSE,
                                 contextMenu = TRUE,
                                 maxRows = 16,
                                 maxRows = 24) %>%
      rhandsontable::hot_context_menu(allowRowEdit = FALSE,
                                      allowColEdit = FALSE) %>%
      rhandsontable::hot_cols(colWidths = 100) %>%
      rhandsontable::hot_rows(rowHeights = 25)
  })
  
  
  
  
  
  
  observeEvent(input$saveSampleMap, 
               ignoreInit = TRUE, {
                 z <- unlist(input$plateDefault$data, recursive = FALSE) 
                 zz <- as.character(z)
                 zz[zz == "NULL"] <- NA 
                 
                 # for some reason rhandsontable hot_to_r not working, implementing own:
                 changed <- base::matrix(zz,
                                         nrow = nrow(sampleMapReactive$rt),
                                         ncol = ncol(sampleMapReactive$rt),
                                         dimnames = list(LETTERS[1:16],1:24),
                                         byrow = T)
                 
                 sampleMapReactive$rt <- as.data.frame(changed, stringsAsFactors = FALSE)
                 
                 
               })
  
  
  
  
  
  sanity <- reactive({
    a <- IDBacApp::path_sanitize(input$newExperimentName)
    gsub(" ","",a)
  })
  
  
  success <- reactiveValues(val = FALSE)
  
  # Call the Spectra processing function when the spectra processing button is pressed
  #----
  observeEvent(input$convertSingleBruker,
               ignoreInit = TRUE,  {
                 
               #  req(length(anyMissing()) == 0)
                 req(!is.null(sanity()))
                 req(sanity() != "")
                 
                 validate(need(any(!is.na(sampleMapReactive$rt)), 
                               "No samples entered into sample map, please try entering them again"))
                 
                 
                 rawFilesLocation <<- rawFilesLocation()
                 inputIds <<- sampleMapReactive$rt
                 
                 # 
                 # function(rawFilesLocation,
                 #          inputIds){
                 #   
                 #   
                 #   aa <- sapply(1:24, function(x) paste0(LETTERS[1:16], x))
                 #   aa <- matrix(aa, nrow = 16, ncol = 24)
                 #   
                 #   spots <<-  IDBacApp::brukerDataSpotsandPaths(brukerDataPath = rawFilesLocation())
                 #   s1 <- base::as.matrix(sampleMapReactive$rt)
                 #   sampleMap <- sapply(spots, function(x) s1[which(aa %in% x)])
                 #   
                 #    
                 # }
                 # 
                 # 
                 # 
                 # 
                 # 
                 
                 
                 IDBacApp::brukerToMzml_popup()
                 
                 forProcessing <- IDBacApp::startingFromBrukerFlex(chosenDir = rawFilesLocation(), 
                                                                   msconvertPath = "",
                                                                   sampleMap = sampleMap,
                                                                   convertWhere = tempMZDir)
                 IDBacApp::popup3()
                 IDBacApp::processMZML(mzFilePaths = forProcessing$mzFile,
                                       sampleIds = forProcessing$sampleID,
                                       sqlDirectory = sqlDirectory$sqlDirectory,
                                       newExperimentName = input$newExperimentName)
                 
                 # Update available experiments
                 availableExperiments$db <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                                                 pattern = ".sqlite",
                                                                                 full.names = FALSE))
                 IDBacApp::popup4()
                 
                 
               })
  
  }

