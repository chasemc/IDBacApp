
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
                          protein){
  
  
  
  
  availableNewSamples <- reactive({
    IDBacApp::availableSampleNames(pool = pool,
                                   protein = protein)
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







#   
#   
#   
#   
#   
#   
#   
#   
#   collapsedPeaks <- reactive({
#     req(input$addSampleChooser$right)
#     
#     query <- DBI::dbSendStatemen("
#                                  SELECT DISTINCT `spectrumSHA`, `Strain_ID`
#                                  FROM `IndividualSpectra`
#                                  WHERE (`proteinPeaks` IS NOT NULL) AND
#                                  WHERE `Strain_ID` = ?",
#                                  .con = conn)
#     
#     DBI::dbBind(query, list(as.vector(as.character(input$addSampleChooser$right))))
#     selectedMeta <- DBI::dbFetch(query)
#     
#     dbClearResult(query)
#     
#     
#     
#   
#     #TODO: Lapply might be looked at and consider replacinng with  parallel::parLapply() 
#     conn <- pool::poolCheckout(pool)
#     
#     temp <- lapply(temp,
#                    function(x){
#                      IDBacApp::collapseProteinReplicates(checkedOutPool = conn,
#                                                          fileshas = x,
#                                                          proteinPercentPresence = input$percentPresenceP,
#                                                          lowerMassCutoff = input$lowerMass,
#                                                          upperMassCutoff = input$upperMass,
#                                                          minSNR = 6)
#                    })
#     
#     pool::poolReturn(conn)
#     return(temp)
#     
#   })
#   
#   
# 
#   
#   
#   
#   return(collapsedPeaks)
#   
# }
# 



#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param pool 
#' @param protein 
#'
#' @return
#' @export
#'
#' @examples
availableSampleNames <- function(pool, protein){
  
  
  conn <- pool::poolCheckout(pool)
  if(protein == TRUE){
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
  
  query <- DBI::dbGetQuery(conn, query)
  pool::poolReturn(conn)
  return(query[ , 1])
  
  
}
