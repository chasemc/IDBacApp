
#' Export samples from database
#'
#' @param id id
#'
#' @return ui
#' 
#'
exportSamples_ui <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    
    div(align = "center",  
        h3("Save/Export Samples"),
        tags$hr(size = 20),
        databaseSelector_UI(ns("dbselector")),   
        tags$hr(size = 20),
        p("Move samples between boxes by clicking the sample's name
          and then an arrow. Samples in the right box will be exported."),
        sampleChooser_UI(ns("chooseNewDBSamples")),
        tags$hr(size = 20),
        actionButton(ns("selectOutDir"),
                     label = "Select where to save files to:"),
        verbatimTextOutput(ns("selectedDir"),
                           placeholder = FALSE),
        tags$hr(size = 20),
        actionButton(ns("exportSpectra"),
                     label = "Export spectra as mzML")
        
    )
  )
  
}




#' exportSamples_server
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param sqlDirectory sqlDirectory$sqlDirectory
#' @param availableExperiments  availableExperiments$db
#'
#' @return NA
#' 
#'

exportSamples_server <- function(input,
                                 output,
                                 session,
                                 sqlDirectory,
                                 availableExperiments){
  
  selectedDB <-  shiny::callModule(databaseSelector_server,
                                   "dbselector",
                                   availableExperiments = availableExperiments,
                                   sqlDirectory = sqlDirectory,
                                   h3Label = "Select an existing experiment to export samples from:")
  
  
  
  chosenSamples <-  shiny::callModule(sampleChooser_server,
                                      "chooseNewDBSamples",
                                      pool = selectedDB$pool,
                                      type = "all")
  
  
  chosenDirectory <- reactiveValues(value = NULL)
  
  observeEvent(input$selectOutDir, {
    chosenDirectory$value <- choose_dir()
    
  })
  output$selectedDir <- renderText({
    req(!is.null(chosenDirectory$value))
    chosenDirectory$value
    
  })
  
  
  observeEvent(input$exportSpectra, {
    
    req(class(selectedDB$pool())[[1]] == "Pool")
    req(length(chosenSamples$chosen) > 0)
    req(dir.exists(chosenDirectory$value))
    
    exportmzML(pool = selectedDB$pool(),
                         sampleIDs = chosenSamples$chosen,
                         saveToDir = chosenDirectory$value)
    
  })
  
}