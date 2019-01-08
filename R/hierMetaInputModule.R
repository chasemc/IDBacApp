

# Module UI function
hierMetaUI <- function(id, label = "Sample Metadata Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  uiOutput(ns("selectMetaColumnUI"))
 

  
  
}



hierMeta <- function(input, output, session, dendrogram, pool) {
  
  conn <- pool::poolCheckout(pool)
  a <- dbListFields(conn, "metaData")
  a <- [-which(a == "Strain_ID")]
  
  output$selectMetaColumnUI <- renderUI({
    ns <- session$ns  
    selectInput(ns("selectMetaColumn"),
                "Select Category",
                as.vector(a)
                )
  })
  
  if(!is.null(input$selectMetaColumn)) {
   
    columnID <- input$selectMetaColumn
    
    query <- glue::glue_sql("SELECT {`columnID`} FROM metaData",
                            .con=conn)
    chosenMeta <- DBI::dbGetQuery(conn, query)
    
    query <- glue::glue_sql("SELECT Strain_ID FROM metaData",
                            .con=conn)
    sampleIds <- DBI::dbGetQuery(conn, query)
    
    pool::poolReturn(conn)
    
    selectedMeta <- base::cbind.data.frame(sampleIds, chosenMeta)
    
    
    selectedMeta <- selectedMeta[sampleIds %in% base::labels(dendrogram), ]
    
    
    cols <- IDBacApp::colorBlindPalette()
    
    
    colsd <- cols[factor(selectedMeta[,2])]
    
    
    
    IDBacApp::colored_dots(colsd,
                           dendrogram,
                           #  rowLabels = names(coloredDend()$bigMatrix),
                           horiz = T,
                           sort_by_labels_order = TRUE)
    
  } 
  
  
}







