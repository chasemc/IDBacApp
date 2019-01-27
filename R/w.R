
sampleChooserUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    shiny::uiOutput(ns("chooseSamples"))
    
  )
}

sampleChooser <- function(input,
                          output,
                          session,
                          pool,
                          whetherProtein = FALSE,
                          allSamples = FALSE){
  
  
  
  
  availableNewSamples <- reactive({
    IDBacApp::availableSampleNames(pool = pool,
                                   whetherProtein = whetherProtein,
                                   allSamples = allSamples)
  })
  
  
  
  # Select samples for input into new databe
  #----
  output$chooseSamples <- renderUI({
    ns <- session$ns
    
    IDBacApp::chooserInput(ns("addSampleChooser"),
                           "Available samples", 
                           "Selected samples",
                           availableNewSamples(),
                           c(),
                           size = 10, 
                           multiple = TRUE
    )
  })
  
  
  print(input$addSampleChooser$right)
  
  return(input$addSampleChooser$right)
}






#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param pool 
#' @param protein 
#'
#' @return
#' @export
#'
#' @examples
availableSampleNames <- function(pool, whetherProtein, allSamples){
  
  
  conn <- pool::poolCheckout(pool)
  
  if (allSamples == TRUE) {
  
    query <- glue::glue_sql("
                          SELECT DISTINCT `Strain_ID`
                            FROM `IndividualSpectra`",
                            .con = conn)
  } else {
  if (protein == TRUE) {
    query <- glue::glue_sql("
                          SELECT DISTINCT `Strain_ID`
                          FROM `IndividualSpectra`
                          WHERE (`proteinPeaks` IS NOT NULL)",
                            .con = conn)
    
  } else {
    
    query <- glue::glue_sql("
                         SELECT DISTINCT `Strain_ID`
                         FROM `IndividualSpectra`
                         WHERE (`smallMoleculePeaks` IS NOT NULL)",
                            .con = conn)
    
  }
    }
    
  query <- DBI::dbGetQuery(conn, query)
  pool::poolReturn(conn)
  return(query[ , 1])
  
  
}
