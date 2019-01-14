
dendDotsUI1 <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::absolutePanel(
    bottom = "0%",
    right = "0%",
    width = "20%",
    fixed = TRUE,
    draggable = TRUE,
    style = "z-index:1002;",
    style = "opacity: 0.80",
    shiny::wellPanel(
      uiOutput(ns("proteDendDots")),
      uiOutput(ns("sampleFactorMapColors")),
      shiny::actionButton("closeLineModification",
                          "Close")
      
    ))
}




dendDotsUI2 <- function(id) {
  ns <- NS(id)
  uiOutput(ns("hclustPlotter"))
}


dendDotsServer <- function(input,
                           output,
                           session,
                           dendrogram,
                           dendTrimmedLabels,
                           pool,
                           plotWidth){
  
  
  
  
  output$sampleFactorMapColors <- renderUI({
    column(3,
           lapply(1:length(levs()),
                  function(x){
                    ns <- session$ns 
                    
                    do.call((colourpicker::colourInput),
                            list(inputId = ns(paste0("factor-",
                                                     gsub(" ",
                                                          "",
                                                          levs()[[x]]))),
                                 label = levs()[[x]],
                                 value = "blue",
                                 allowTransparent = T))})
           
    )
  })
  
  
  levs <- reactive({
    req(input$selectMetaColumn)
    conn <- pool::poolCheckout(pool)
    dendLabs <- labels(dendrogram)
    query <- DBI::dbSendStatement("SELECT *
                                  FROM metaData
                                  WHERE `Strain_ID` = ?",
                                  con=conn)
    DBI::dbBind(query, list(dendLabs))
    selectedMeta <- DBI::dbFetch(query)
    
    DBI::dbClearResult(query)
    
    selectedMeta <- selectedMeta[ , colnames(selectedMeta) %in% input$selectMetaColumn]
    pool::poolReturn(conn)
    return(unique(selectedMeta))
  })
  
  
  output$proteDendDots <- renderUI({
    ns <- session$ns 
    conn <- pool::poolCheckout(pool)
    a <- DBI::dbListFields(conn, "metaData")
    a <- a[-which(a == "Strain_ID")]
    
    selectInput(ns("selectMetaColumn"),
                "Select Category",
                as.vector(a))
    
  })
  
  
  
  colorsChosen <- reactive({
    sapply(levs(),
           function(x){ 
             
             input[[paste0("factor-", gsub(" ", "", x))]]
             
           })
    
  })
  
  observe({    req(!is.null(colorsChosen()[[1]]))
    ns <- session$ns
    
    output$hclustPlotter <- renderUI(   plotOutput(ns("hclustPlot"))   )
    
    
    
    output$hclustPlot <- shiny::renderPlot({
      
      
      # dendTrimmedLabels <- shiny::callModule(IDBacApp::colordendLines,
      #                                        "proteinDendLines",
      #                                        dendrogram = dendrogram)
      dendTrimmedLabels <- shiny::callModule(IDBacApp::colordendLabels,
                                             "proteinDendLabels",
                                             dendrogram = dendTrimmedLabels)
      
      labs <- base::strtrim(labels(dendTrimmedLabels),10)
      
      labels(dendTrimmedLabels) <- labs
      par(mar = c(5, 5, 5, plotWidth))
      plot(dendTrimmedLabels, horiz = TRUE)
      
      IDBacApp::runDendDots(rawDendrogram = dendrogram,
                            trimdLabsDend = dendTrimmedLabels,
                            pool = pool, 
                            columnID = input$selectMetaColumn,
                            colors = colorsChosen(),
                            text_shift = 1) 
      
    })
  })
  
  
  
  
  
  
  
}
