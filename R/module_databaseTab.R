



#' Layout for the databaseTabUI
#'
#' @param id namespace
#'
#' @return ui for database UI
#' @export
#'
databaseTabUI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
  
    column(width = 4,
           fluidRow(
             column(width = 12,
                    align = "center",
                    IDBacApp::databaseSelector_UI(ns("databaseSelector"))
             )
           )
    ),
    column(width = 8,
           fluidRow(
           #  shinyBS::bsCollapse(id = ns("collapseSQLInstructions"),
                               #  open = "Panel 1",
                              #   shinyBS::bsCollapsePanel(h4("Open\\Close Instructions", 
                                           #                  align = "center"),
                                                          tags$b("What is an \"experiment\" in IDBac?"),
                                                          tags$ul(
                                                            tags$li("A user-defined group of samples that were analyzed by MALDI MS."),
                                                            tags$li("Physically, each experiment is a separate \"SQLite\" database that can be shared between colleagues
                                                                  or submitted alongside a manuscript."),
                                                            tags$li("Experiments contain the converted mzML file, instrument and data collection info (if found), 
                                                                  processed spectra, as well as any sample information input into IDBac by you.")),
                                                          tags$b("What does this mean for me?"),
                                                          
                                                          tags$ul(
                                                            tags$li("Experiments are how you organize your data."),
                                                            tags$li("Experiments should only contain the samples you want to analyze."),
                                                            tags$li("It is possible to \"mix and match\" samples from different experiments to create new experiments.")
                                                          ),
                                                          tags$b("Begin analysis by selecting an available experiment to the right."), 
                                                          br(),
                                                          br(),
                                                          tags$b("You also have the option, below, to:"),
                                                          tags$ul(
                                                            tags$li("Create new experiments using samples from the selected experiment"),
                                                            tags$li("Add information about strains (for coloring plots later, or just as a record)")
                                                            
                                                          )
                               ##  )
                                 
            # )
           ),
           fluidRow(
            # shinyBS::bsCollapse(id = ns("modifySqlCollapse"),
                          #       shinyBS::bsCollapsePanel(h4("Click here to modify the selected experiment", align = "center"),  
                                                          tabsetPanel(id = ns("ExperimentNav"), 
                                                                      tabPanel("Create an experiment, pulling samples from the selected experiment",
                                                                               value = "experiment_mixMatch_tab",
                                                                               column(12, align = "center",
                                                                                      style = "background-color: #7777770d",
                                                                                      offset = 1,
                                                                                  IDBacApp::transferToNewDB_UI(ns("transferToNewDB"))
                                                                               )
                                                                      ),
                                                                      tabPanel("Add/modify information about samples",
                                                                               value = "experiment_metaData_tab",
                                                                       IDBacApp::updateMeta_UI(ns("updateMeta"))
                                                                                  
                                                                      )
                                                          ) 
                                 )
            # )
          # )
           
    )
  
  )
  
}





databaseTabServer <- function(input,
                              output,
                              session,
                              workingDirectory,
                              availableExperiments){
  
  #outputs reactive inputs, access via $
  selectedDB <- callModule(IDBacApp::databaseSelector_server,
                            "databaseSelector",
                            h3Label = "First, select an experiment:",
                            availableExperiments = availableExperiments,
                            workingDirectory = workingDirectory)

  
  
  
  
  
# Update Metadata ---------------------------------------------------------



  callModule(IDBacApp::updateMeta_server,
             "updateMeta",
             pool = selectedDB$userDBCon,
             selectedDB = selectedDB$inputs)
  
  
  
  
  callModule(IDBacApp::transferToNewDB_server,
             "transferToNewDB",
             pool = selectedDB$userDBCon,
             workingDirectory = workingDirectory,
             selectedDB  = selectedDB$inputs)
  

  
  
  
  
  # Collapsable instruction panels
  observeEvent(input$styleSelect, 
               ignoreInit = TRUE, {
                 ns <- session$ns
                 updateCollapse(session, ns("collapseSQLInstructions"))
                 updateCollapse(session, ns("modifySqlCollapse"))
                 isolate(
                   updateCollapse(session, ns("collapseSQLSelector"))
                 )
               }
  )
  
  
  
  

  #This "observe" event creates the UI element for analyzing a single MALDI plate, based on user-input.
  #----
  observe({
    if (is.null(input$startingWith)) { } else if (input$startingWith == 3) {
      output$mzConversionUI <- renderUI({
        IDBacApp::beginWithMZ("beginWithMZ")
      })
    }
  })
  
  
  # Reactive variable returning the user-chosen location of the raw MALDI files as string
  #----
  mzmlRawFilesLocation <- reactive({
    if (input$mzmlRawFileDirectory > 0) {
      IDBacApp::choose_dir()
    }
  })
  
  
  # Creates text showing the user which directory they chose for raw files
  #----
  output$mzmlRawFileDirectory <- renderText({
    if (is.null(mzmlRawFilesLocation())) {
      return("No Folder Selected")
    } else {
      folders <- NULL
      
      findmz <- function(){
        # sets time limit outside though so dont use yet setTimeLimit(elapsed = 5, transient = FALSE)
        return(list.files(mzmlRawFilesLocation(),
                          recursive = TRUE,
                          full.names = FALSE,
                          pattern = "\\.mz"))
        setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
        
      }
      
      
      # Get the folders contained within the chosen folder.
      foldersInFolder <- tryCatch(findmz(),
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
  
  
  
  
  return(list(pool = selectedDB$userDBCon,
              move = selectedDB$inputs)
              )
  
}













