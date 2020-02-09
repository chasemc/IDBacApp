
#' databaseSelector_UI
#'
#' @param id namespace
#'
#' @return modal ui
#' 
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
#' @param sqlDirectory sqlDirectory 
#'
#' @return filepath of selected database
#' 
#'
databaseSelector_server <- function(input,
                                    output,
                                    session,
                                    h3Label = h3("First, select an experiment:"),
                                    availableExperiments,
                                    sqlDirectory){
  
  
  output$availableDB <- renderUI({
    ns <- session$ns
    selectInput(ns("selectExperiment"),
                label = h3Label,
                choices = c("None", availableExperiments$db),
                selected = "None",
                width = "50%"
    )
  })
  
  output$selectedSQLText <- renderPrint({
    
    fileNames <- tools::file_path_sans_ext(list.files(sqlDirectory$sqlDirectory,
                                                      pattern = ".sqlite",
                                                      full.names = FALSE))
    filePaths <- list.files(sqlDirectory$sqlDirectory,
                            pattern = ".sqlite",
                            full.names = TRUE)
    
    filePaths[which(fileNames == input$selectExperiment)]
    
  })
  
  userDBCon <- reactive({
    
    req(!is.null(input$selectExperiment))
    req(nchar(input$selectExperiment) > 0)
    req(input$selectExperiment != "None")
    validate(need(length(input$selectExperiment) == length(sqlDirectory$sqlDirectory), 
                  "databaseTabServer: userDBCon, idbac_connect inputs are different lengths."))
    
    # pool will create a new sqlite if one doesn't exist, so let's stop that from happening here:
    req(file.exists(file.path(sqlDirectory$sqlDirectory, 
                              paste0(input$selectExperiment, ".sqlite"))))
    
    z <- idbac_connect(fileName = input$selectExperiment,
                              filePath = sqlDirectory$sqlDirectory)[[1]]
    
    
    q <- c("xml",
           "locale",
           "mass_index",
           "metadata",
           "spectra",
           "version")
    
    req(all(q %in% tolower(DBI::dbListTables(z))))
    
    return(z)
    
    
    
  })  
  
  return(list(userDBCon = userDBCon,
              inputs = input))
}