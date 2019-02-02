
#' databaseSelector_UI
#'
#' @param id namespace
#'
#' @return modal ui
#' @export
#'
databaseSelector_UI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    uiOutput(ns("availableDB")),
    p("Location of experiment file:", align = "center"),
    verbatimTextOutput(ns("selectedSQLText"),
                       placeholder = TRUE)
  )
}


#' databaseSelector_server
#'
#' @param input  input 
#' @param output  output 
#' @param session session 
#' @param h3Label h3Label 
#' @param availableExperiments availableExperiments 
#'
#' @return filepath of selected database
#' @export
#'
databaseSelector_server <- function(input,
                                    output,
                                    session,
                                    h3Label = "First, select an experiment:",
                                    availableExperiments,
                                    workingDirectory){
  
  
  output$availableDB <- renderUI({
    ns <- session$ns
    selectInput(ns("selectExperiment"),
                label = h3(h3Label),
                choices = availableExperiments$db,
                selected = NULL,
                width = "50%"
    )
  })
  
  output$selectedSQLText <- renderPrint({
    
    fileNames <- tools::file_path_sans_ext(list.files(workingDirectory,
                                                      pattern = ".sqlite",
                                                      full.names = FALSE))
    filePaths <- list.files(workingDirectory,
                            pattern = ".sqlite",
                            full.names = TRUE)
    
    filePaths[which(fileNames == input$selectExperiment)]
    
  })
 
   userDBCon <- reactive({
    
    req(!is.null(input$selectExperiment))
    validate(need(length(input$selectExperiment) == length(workingDirectory), 
                  "databaseTabServer: userDBCon, createPool inputs are different lengths."))
    IDBacApp::createPool(fileName = input$selectExperiment,
                         filePath = workingDirectory)[[1]]
    
    
  })  
  
 return(list(userDBCon = userDBCon,
             inputs = input))
}