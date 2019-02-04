
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
    wellPanel(
      align = "center",
      
      h3("Starting with a Single MALDI Plate of Raw Data", align = "center"),
      p(strong("1: Enter a name for this new experiment")),
      p("This will become a filename, so avoid non-valid characters
       as they will be removed."),
      p("Hint: Intead of \" \", use \"_\"."),
      textInput(ns("newExperimentName"),
                label = "",
                width = "50%"),
      verbatimTextOutput(ns("newExperimentNameText"),
                         placeholder = TRUE),
      tags$hr(size = 20),
      br(),
      p(strong("2: Click to select the location of your RAW data"), align = "center"),
      actionButton(ns("rawFileDirectory"),
                   label = "Raw Data Folder"),
      verbatimTextOutput(ns("rawFileDirectoryText"),
                         placeholder = TRUE),
      tags$hr(size = 20),
      br(),
      p(strong("3:", "Fill in the Sample-ID spreadsheet.")),
      
      actionButton(ns("showSampleMap"), "Click to name samples"),
      br(),
      p(strong("Missing sample IDs for the following spots:")),
      shiny::verbatimTextOutput(ns("missingSampleNames"), placeholder = TRUE),
      p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
      actionButton(ns("convertSingleBruker"),
                   label = "Process Data"),
      tags$hr(size = 20)
    )
  )
}





#' convertOneBruker_Server
#'
#' @param input NS
#' @param output NS
#' @param session NS
#' @param tempMZDir tempMZDir 
#' @param sqlDirectory sqlDirectory
#'
#' @return
#' @export
#'
convertOneBruker_Server <- function(input,
                                    output,
                                    session,
                                    tempMZDir,
                                    sqlDirectory){
  
  
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  rawFilesLocation <- reactive({
    if (input$rawFileDirectory > 0) {
      IDBacApp::choose_dir()
    }
  })
  
  
  output$newExperimentNameText <- renderText({
    a <- gsub(" ", "", IDBacApp::path_sanitize(input$newExperimentName))
    
    if (a == "") {
      "Enter a valid file name"
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
    
    aa <- sapply(1:24, function(x) paste0(LETTERS[1:16], x))
    aa <- matrix(aa, nrow = 16, ncol = 24)
    
    
    spots <- IDBacApp::brukerDataSpotsandPaths(brukerDataPath = rawFilesLocation())
    s1 <- base::as.matrix(sampleMapReactive$rt)
    b <- sapply(spots, function(x) s1[which(aa %in% x)])
    as.character(spots[which(is.na(b))])
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
                 showModal(modalDialog(footer = actionButton(ns("saveSampleMap"), "Save"),{
                   tagList(
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
  
  # Call the Spectra processing function when the spectra processing button is pressed
  #----
  observeEvent(input$convertSingleBruker,
               ignoreInit = TRUE,  {
                 
                 req(length(anyMissing()) == 0)
                 req(!is.null(sanity()))
                 req(sanity() != "")
                 
                 validate(need(any(!is.na(sampleMapReactive$rt)), 
                               "No samples entered into sample map, please try entering them again"))
                 aa <- sapply(1:24, function(x) paste0(LETTERS[1:16], x))
                 aa <- matrix(aa, nrow = 16, ncol = 24)
                 
                 spots <-  brukerDataSpotsandPaths(brukerDataPath = rawFilesLocation())
                 s1 <- base::as.matrix(sampleMapReactive$rt)
                 sampleMap <- sapply(spots, function(x) s1[which(aa %in% x)])
                 
                 IDBacApp::brukerToMzml_popup()
                 
                 forProcessing <- startingFromBrukerFlex(chosenDir = rawFilesLocation(), 
                                                         msconvertPath = "",
                                                         sampleMap = sampleMap,
                                                         tempDir = tempMZDir)
                 
                 
                 
                 IDBacApp::popup3()
                 
                 
                 
                 IDBacApp::processMZML(mzFilePaths = forProcessing$mzFile,
                                       sampleIds = forProcessing$sampleID,
                                       sqlDirectory = sqlDirectory,
                                       newExperimentName = input$newExperimentName)
                 
                 
                 
                 
                 
                 
                 IDBacApp::popup4()
               })
  
  
  
  
  
}



