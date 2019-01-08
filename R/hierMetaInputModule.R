

# Module UI function
hierMetaUI <- function(id, label = "Sample Metadata Input") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  uiOutput(ns("selectMetaColumnUI"))
 

  
  
}



hierMeta <- function(input, output, session, pool) {
  
  conn <- pool::poolCheckout(pool)
  output$selectMetaColumnUI <- renderUI({
    ns <- session$ns  
    selectInput(ns("selectMetaColumn"),
                "Select Category",
                dbListFields(conn, "metaData"))
  })
  
  return(reactive({
    validate(need(input$selectMetaColumn, FALSE))
    
    columnID <- input$selectMetaColumn
    
    query <- glue::glue_sql("SELECT {`columnID`} FROM metaData",
    chosenMeta <- DBI::dbGetQuery(conn, query)
    
    query <- glue::glue_sql("SELECT Strain_ID FROM metaData",
                            .con=conn)
    sampleIds <- DBI::dbGetQuery(conn, query)
    
    pool::poolReturn(conn)
    
    cbind.data.frame(sampleIds, chosenMeta)
  })
  )
}







