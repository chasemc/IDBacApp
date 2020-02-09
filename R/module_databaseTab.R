#' Layout for the databaseTabUI
#'
#' @param id namespace
#'
#' @return ui for database UI
#' 
#'
databaseTabUI <- function(id) {
  ns <- shiny::NS(id)
  
  databaseTabUIFunc(ns)
    
  
  
}





#' databaseTabServer
#'
#' @param input module
#' @param output  module
#' @param session  module
#' @param sqlDirectory  sqlDirectory
#' @param availableExperiments availableExperiments 
#'
#' @return .
#' 
#'

databaseTabServer <- function(input,
                              output,
                              session,
                              sqlDirectory,
                              availableExperiments){
  
  #outputs reactive inputs, access via $
  selectedDB <- callModule(databaseSelector_server,
                           "databaseSelector",
                           h3Label = tags$h4("Select an experiment ", br(), "to work with:"),
                           availableExperiments = availableExperiments,
                           sqlDirectory = sqlDirectory)
  
  
  shiny::callModule(experimentSummary_Server,
                    "experimentSummary",
                    pool = selectedDB$pool)
  
  
  
  # Update Metadata ---------------------------------------------------------
  
  
  
  callModule(updateMeta_server,
             "updateMeta",
             pool = selectedDB$pool,
             selectedDB = selectedDB$inputs)
  
  
  
  
  callModule(transferToNewDB_server,
             "transferToNewDB",
             sqlDirectory = sqlDirectory,
             availableExperiments = availableExperiments)
  
  
  
  
  
  
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
  
  
  
  
  # Export Data -------------------------------------------------------------
  
  shiny::callModule(exportSamples_server,
                    "exportSamples",
                    availableExperiments = availableExperiments,
                    sqlDirectory = sqlDirectory)
  
  
  
  
  
  
  
  
  
  
  return(list(pool = selectedDB$pool,
              move = selectedDB$inputs)
  )
  
}








#' databaseTabUIFunc
#'
#' @param ns shiny namespace
#'
#' @return ui
#' 
#'

databaseTabUIFunc <- function(ns) {


tagList(
  
  column(width = 4,
         fluidRow(
           column(width = 12,
                  align = "center",
                  wellPanel(    
                    databaseSelector_UI(ns("databaseSelector"))
                  )
           )
         )
  ),
  column(width = 8,
         fluidRow(
           bsCollapse(id = ns("collapseSQLInstructions"),
                                open = "Panel 1",
                                bsCollapsePanel(h4("Open\\Close Instructions", 
                                                             align = "center"),
                                                          tags$b("What is an \"experiment\" in IDBac?"),
                                                          tags$ul(
                                                            tags$li("A user-defined group of samples that were analyzed by MALDI MS."),
                                                            tags$li("Each experiment is a separate \"SQLite\" database that can be shared between colleagues
                                                                    or submitted alongside a manuscript."),
                                                            tags$li("Experiments contain the converted mzML file, instrument and data collection info (if found), 
                                                                    processed spectra, as well as any sample information input into IDBac by you.")),
                                                          tags$b("What does this mean for me?"),
                                                          
                                                          tags$ul(
                                                            tags$li("Experiments should only contain the samples you want to analyze."),
                                                            tags$li("It is possible to \"mix and match\" samples from different experiments to create new experiments.
                                                                    However it should be noted that is not possible to remove samples from an experiment.")
                                                            ),
                                                          tags$b("Begin analysis by selecting a previously-created experiment to the left or use the menus below to modify experiments.")
                                                          
                                                          )
                                                            )
  ),
  fluidRow(
    bsCollapse(id = ns("modifySqlCollapse"),
                         bsCollapsePanel(h4("Click here to modify the selected experiment", align = "center"),  
                                                   tabsetPanel(id = ns("ExperimentNav"), 
                                                               
                                                               tabPanel("Add/modify information about samples",
                                                                        value = "experiment_metaData_tab",
                                                                        updateMeta_UI(ns("updateMeta"))
                                                                        
                                                               ),
                                                               tabPanel("Experiment Summary",
                                                                        value = "experiment_summary_tab",
                                                                        experimentSummary_UI(ns("experimentSummary"))
                                                               ) )
                         )
    )
  ),
  fluidRow(
    bsCollapse(id = ns("createDbFromDb"),
                         bsCollapsePanel(h4("Click here to copy samples from an existing experiment to a new experiment",
                                                      align = "center"),  
                                                   wellPanel(
                                                     
                                                     transferToNewDB_UI(ns("transferToNewDB"))
                                                   )
                                                   
                         )
    )
  ),
  fluidRow(
    bsCollapse(id = ns("exportmzml"),
                         bsCollapsePanel(h4("Click here to export samples as mzML",
                                                      align = "center"),  
                                                   wellPanel(
                                                     exportSamples_ui(ns("exportSamples"))
                                                   )
                                                   
                         )
    )
  )
  
  
  
  
                                )
)
}
