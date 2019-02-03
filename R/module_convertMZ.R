
#' beginWithMZ
#'
#' @param id NA
#'
#' @return NA
#' @export
#'
#' @examples NA
convertMZ_UI <- function(id){
  ns <- NS(id)
  tagList(
    h3("Starting with mzML or mzXML Data:"),
    p(strong("1: Enter a filename for this new experiment")),
    p("Only numbers, \"_\", and A-Z. Shouldn't start with a number."),
    textInput(ns("newExperimentName"),
              label = "",
              width = "50%"),
    tags$hr(size = 20),
    
    br(),
    p(strong("2: Click to select the location of your mzML files"),
      align = "center"),
    actionButton(ns("mzmlRawFileDirectory"),
                 label = "Raw Data Folder"),
    verbatimTextOutput(ns("mzmlRawFileDirectorytext"),
                       placeholder = TRUE),
    tags$hr(size = 20),
    br(),
    p("Samples will be named according to the file name of the provided files"),
    br(),
    p(strong("4:","Click \"Process Data\" to begin spectra conversion.")),
    actionButton(ns("runMsconvert"),
                 label = "Process Data"),
    tags$hr(size = 20)
  )
  
  
}


convertMZ_Server <-  function(input,
                              output,
                              session,
                              sqlDirectory){
  
  
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  mzmlRawFilesLocation <- reactive({
    if (input$mzmlRawFileDirectory > 0) {
      IDBacApp::choose_dir()
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
  
  observeEvent(input$runMsconvert, {
    IDBacApp::popup3()
    
    mzFilePaths <- IDBacApp::findmz(mzmlRawFilesLocation(),
                                    recursive = TRUE,
                                    full = TRUE)
    
    
    IDBacApp::processMZML(mzFilePaths = mzFilePaths,
                          sampleIds = base::basename(tools::file_path_sans_ext(mzFilePaths)),
                          sqlDirectory = sqlDirectory,
                          newExperimentName = input$newExperimentName)
      
      

    IDBacApp::popup4() 
  })
  
}
