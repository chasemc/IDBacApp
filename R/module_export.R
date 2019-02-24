
#' Export samples from database
#'
#' @param id id
#'
#' @return ui
#' @export
#'
exportSamples_ui <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    
    div(align = "center",  
        h3("Export Samples"),
        tags$hr(size = 20),
        IDBacApp::databaseSelector_UI(ns("dbselector")),   
        tags$hr(size = 20),
        p("Move samples between boxes by clicking the sample's name
          and then an arrow. Samples in the right box will be exported."),
        IDBacApp::sampleChooser_UI(ns("chooseNewDBSamples")),
        tags$hr(size = 20),
        actionButton(ns("selectOutDir"),
                     label = "Select where to save files to:"),
        verbatimTextOutput(ns("selectedDir"),
                           placeholder = FALSE),
        tags$hr(size = 20),
        actionButton(ns("exportSpectra"),
                     label = "Export Spectra as mzML")
        
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
#' @export
#'

exportSamples_server <- function(input,
                                 output,
                                 session,
                                 sqlDirectory,
                                 availableExperiments){
  
  selectedDB <-  shiny::callModule(IDBacApp::databaseSelector_server,
                                   "dbselector",
                                   availableExperiments = availableExperiments,
                                   sqlDirectory = sqlDirectory,
                                   h3Label = "Select an existing experiment to copy samples from:")
  
  
  
  chosenSamples <-  shiny::callModule(IDBacApp::sampleChooser_server,
                                      "chooseNewDBSamples",
                                      pool = selectedDB$userDBCon,
                                      allSamples = TRUE,
                                      whetherProtein = FALSE)

    observe(pl<<-chosenSamples)
  
  copyingDbPopup <- reactive({
    showModal(modalDialog(
      title = "Important message",
      glue::glue("When file-export is complete you can find the files at: /n",
                 {chosenDirectory$value}),
      br(),
      easyClose = FALSE, 
      size = "l",
      footer = ""))
  })
  
  
  chosenDirectory <- reactiveValues(value = NULL)
  
  observeEvent(input$selectOutDir, {
    chosenDirectory$value <- IDBacApp::choose_dir()
    
  })
  output$selectedDir <- renderText({
    req(!is.null(chosenDirectory$value))
    chosenDirectory$value
    
  })
  
  
  observeEvent(input$exportSpectra, {
  
    a1 <<- selectedDB$userDBCon()
    a2 <<- chosenSamples$chosen
    a3 <<- chosenDirectory$value
    req(class(selectedDB$userDBCon())[[1]] == "Pool")
    req(length(chosenSamples$chosen) > 0)
    req(dir.exists(chosenDirectory$value))
    
    exportmzML(userDBCon = selectedDB$userDBCon(),
             sampleIDs = chosenSamples$chosen,
             saveToDir = chosenDirectory$value)
  
  })
  
}