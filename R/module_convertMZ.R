
#' beginWithMZ
#'
#' @param id NA
#'
#' @return NA
#' @export
#'

convertMZ_UI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Starting with mzML or mzXML Data:"),
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
    p(strong("2: Click to select the location of your mzML files"),
      align = "center"),
    actionButton(ns("mzmlRawFileDirectory"),
                 label = "Raw Data Folder"),
    verbatimTextOutput(ns("mzmlRawFileDirectorytext"),
                       placeholder = TRUE),
    tags$hr(size = 20),
    p("Samples will be named according to the file name of the provided files"),
    br(),
    p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
    actionButton(ns("runMsconvert"),
                 label = "Process Data"),
    tags$hr(size = 20)
  )
  
  
}


#' convertMZ_Server
#'
#' @param input module
#' @param output module
#' @param session module
#' @param sqlDirectory sqlDirectory 
#'
#' @return .
#' @export
#'

convertMZ_Server <-  function(input,
                              output,
                              session,
                              sqlDirectory){
  
  
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  mzmlRawFilesLocation <- reactive({
    if (input$mzmlRawFileDirectory > 0) {
      loc <- IDBacApp::choose_dir()
      
      if(!is.na(loc)){
        return(loc)
      }
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
  output$mzmlRawFileDirectorytext <- renderText({
    if (is.null(mzmlRawFilesLocation())) {
      return("No Folder Selected")
    } else {
      folders <- NULL
      
      
      
      
      # Get the folders contained within the chosen folder.
      foldersInFolder <- tryCatch(IDBacApp::findmz(mzmlRawFilesLocation(),
                                                   recursive = TRUE,
                                                   full = FALSE),
                                  error = function(x) paste("Timed out"),
                                  finally = function(x) x)
      
      if (foldersInFolder == "Timed out") {
        return("Timed out looking for mzML/mzXML files. This can happen if the folder you 
             selected has lots of folders within it... because IDBac looks through all 
             of them for mzML/mzXML files.")}else{
               
               for (i in 1:length(foldersInFolder)) {
                 # Creates user feedback about which raw data folders were chosen.  Individual folders displayed on a new line "\n"
                 folders <- paste0(folders, 
                                   "\n",
                                   basename(foldersInFolder[[i]]))
               }
               return(folders)
             }}
    
    
  })
  
  
  sanity <- reactive({
    a <- IDBacApp::path_sanitize(input$newExperimentName)
    gsub(" ","",a)
  })
  
  observeEvent(input$runMsconvert, {
    
    req(!is.null(mzmlRawFilesLocation()))
    req(!is.null(sanity()))
    req(sanity() != "")
    
    IDBacApp::popup3()
    
    mzFilePaths <- IDBacApp::findmz(mzmlRawFilesLocation(),
                                    recursive = TRUE,
                                    full = TRUE)
    
    print(sqlDirectory$sqlDirectory)
    IDBacApp::processMZML(mzFilePaths = mzFilePaths,
                          sampleIds = base::basename(tools::file_path_sans_ext(mzFilePaths)),
                          sqlDirectory = sqlDirectory$sqlDirectory,
                          newExperimentName = sanity())
    
    
    
    IDBacApp::popup4() 
  })
  
}
