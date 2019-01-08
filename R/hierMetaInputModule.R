

# Module UI function
hierMetaUI <- function(id, label = "Sample Metadata Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  uiOutput(ns("selectMetaColumnUI"))
  
  
  
  
}



hierMeta <- function(input, output, session, dendrogram, pool) {

  
  
  output$selectMetaColumnUI <- renderUI({
    conn <- pool::poolCheckout(pool)
    a <- dbListFields(conn, "metaData")
    a <- a[-which(a == "Strain_ID")]
    ns <- session$ns  
    selectInput(ns("selectMetaColumn"),
                "Select Category",
                as.vector(a)
    )
  })
  
  return(reactive({
    
    dendLabs <- labels(dendrogram)
    conn <- pool::poolCheckout(pool)
   
    if(!is.null(input$selectMetaColumn)) {
    
    columnID <- input$selectMetaColumn
    
    
    query <- DBI::dbSendStatement("SELECT *
                                  FROM metaData
                                  WHERE `Strain_ID` = ?",
                                  con=conn)
    DBI::dbBind(query, list(dendLabs))
    selectedMeta <- DBI::dbFetch(query)
    dbClearResult(query)
    
    
    selectedMeta <- base::cbind.data.frame(ids = selectedMeta$Strain_ID,
                                           meta = selectedMeta[,colnames(selectedMeta) %in% columnID])
    
    
    cols <- IDBacApp::colorBlindPalette()
    
    
    colsd <- cols[factor(selectedMeta[,2])]
    
    
    
    IDBacApp::colored_dots(colsd,
                           dendrogram,
                           #  rowLabels = names(coloredDend()$bigMatrix),
                           horiz = T,
                           sort_by_labels_order = TRUE)
    
    } 
  }))
  
  
}







