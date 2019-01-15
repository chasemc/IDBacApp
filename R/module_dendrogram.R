
#' dendDotsUI
#'
#' @param id  namespace id
#'
#' @return shiny UImodule
#' @export
#'

dendDotsUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPaneldendDots"))
  
}

#' colordendLabelsUI
#'
#' @param id namespace id
#'
#' @return ui for coloring dendrogram labels
#' @export
#'

colordendLabelsUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPanelDendLabels"))
}



#' colordendLinesUI
#'
#' @param id  namespace id
#'
#' @return shiny UImodule
#' @export
#'

colordendLinesUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("absPanelDendLines"))
  
}




#' addDotsActionUI
#'
#' @param id namespace id
#'
#' @return shiny UImodule
#' @export
#'
addDotsActionUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("openDendots"), "Click to add Dots"),
    actionButton(ns("openLineMod"), "Click to modify lines"),
    actionButton(ns("openLabelMod"), "Click to modify labels")
  )
}

#' plotHier
#'
#' @param id namespace id
#'
#' @return shiny UImodule
#' @export
#'
plotHier <- function(id) {
  ns <- shiny::NS(id)
  plotOutput(ns("hierOut"))
  
}








#' Title
#'
#' @param input NA
#' @param output NA
#' @param session NA
#' @param dendrogram NA
#' @param dendTrimmedLabels NA
#' @param pool NA
#' @param plotWidth NA
#'
#' @return NA
#' @export
#'

dendDotsServer <- function(input,
                           output,
                           session,
                           dendrogram,
                           pool,
                           plotWidth,
                           plotHeight){
  
  
  observeEvent(input$closeDendDots, {
    output$absPaneldendDots <- renderUI({
      # Intentionally Blank
    })
    
    
  })  
  
  
  
  observeEvent(input$openDendots, ignoreInit = T ,ignoreNULL = T, {
    ns <- session$ns
    
    output$absPaneldendDots <- renderUI(
      
      shiny::absolutePanel(
        bottom = "0%",
        right = "0%",
        ##   width = "20%",
        fixed = F,
        draggable = TRUE,
        style = "z-index:1002;",
        style = "opacity: 0.80",
        shiny::wellPanel(class = "dendDots_WellPanel",
                         fluidRow(
                           fluidRow(
                             uiOutput(ns("proteDendDots")),
                             uiOutput(ns("sampleFactorMapColors"))),
                           shiny::actionButton(ns("closeDendDots"),
                                               "Close")
                         )
        ))
      
    )
  })
  
  
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
  
  
  observeEvent(input$closeDendLabels, {
    output$absPanelDendLabels <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  
  observeEvent(input$openLabelMod, ignoreInit = T ,ignoreNULL = T, {
    
    ns <- session$ns
    
    output$absPanelDendLabels <- renderUI(
      
      shiny::absolutePanel(
        bottom = "0%",
        right = "0%",
        width = "20%",
        fixed = TRUE,
        draggable = TRUE,
        style = "z-index:1002;",
        style = "opacity: 0.80",
        shiny::wellPanel(
          shiny::h4("Adjust Dendrogram Labels"),
          shiny::selectInput(ns("colorByLabels"),
                             "Color By:",
                             c("None" = "none",
                               "Choose Number of Groups" = "groups",
                               "Color by cutting at height" = "height",
                               "Color by sample info" = "metadata"
                             ),
                             selected = "groups"
          ),
          shiny::conditionalPanel(
            condition = "input.colorByLabels == 'height'", ns = ns,
            shiny::numericInput(ns("cutHeightLabels"),
                                label = shiny::h5(shiny::strong("Cut Tree at Height")),
                                value = 0,
                                step = 0.1,
                                min = 0)
            
            
          ),
          shiny::conditionalPanel(
            condition = "input.colorByLabels == 'groups'", ns = ns,
            shiny::numericInput(ns("chosenKLabels"),
                                label = shiny::h5(shiny::strong("Choose the number of groups")),
                                value = 1,
                                step = 1,
                                min = 1)
          ),
          
          shiny::numericInput(ns("dendLabelSize"),
                              "Label Size",
                              value = 1,
                              min = 0,
                              max = 5,
                              step = .1
          ),
          
          shiny::actionButton(ns("closeDendLabels"),
                              "Close")
          
        )
      )
    )
  }
  
  )
  
  
  
  
  
  observeEvent(input$closeDendLines, {
    output$absPanelDendLines <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  observeEvent(input$openLineMod, ignoreInit = T ,ignoreNULL = T, {
    ns <- session$ns
    
    output$absPanelDendLines <- renderUI(
      shiny::absolutePanel(
        bottom = "50%",
        right =  "0%",
        width = "20%",
        fixed = TRUE,
        draggable = TRUE,
        style = "z-index:1002;",
        style = "opacity: 0.80",
        shiny::wellPanel(
          shiny::h4("Adjust Dendrogram Lines"),
          shiny::selectInput(ns("colorByLines"),
                             "Color By:",
                             c("None" = "none",
                               "Choose Number of Groups" = "groups",
                               "Color by cutting at height" = "height",
                               "Color by sample info" = "metadata"
                             ),
                             selected = "groups"
          ),
          shiny::conditionalPanel(
            condition = "input.colorByLines == 'height'", ns = ns,
            shiny::numericInput(ns("cutHeightLines"),
                                label = shiny::h5(shiny::strong("Cut Tree at Height")),
                                value = 0,
                                step = 0.1,
                                min = 0)
            
            
          ),
          shiny::conditionalPanel(
            condition = "input.colorByLines == 'groups'", ns = ns,
            shiny::numericInput(ns("chosenKLines"),
                                label = shiny::h5(shiny::strong("Choose the number of groups")),
                                value = 1,
                                step = 1,
                                min = 1)
          ),
          
          shiny::numericInput(ns("dendLineWidth"),
                              "Line Width",
                              value = 1,
                              min = 1,
                              max = 10,
                              step = 1
          ),
          
          shiny::actionButton(ns("closeDendLines"),
                              "Close")
          
        )
      )
      
    )
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  dendro <- reactive({
    
    
    if(!is.null(input$colorByLabels)){
      
      dendrogram <- IDBacApp::changeDendPartColor(dendrogram = dendrogram,
                                                  colorBy = input$colorByLabels,
                                                  colorBlindPalette = colorBlindPalette(),
                                                  cutHeight = input$cutHeightLabels,
                                                  chosenK = input$chosenKLabels,
                                                  part = "labels")
    }
    
    if(!is.null(input$dendLabelSize)){
      dendrogram <- IDBacApp::changeDendPartSize(dendrogram = dendrogram,
                                                 dendPartSize = input$dendLabelSize,
                                                 part = "labels")
    }
    
    
    if(!is.null(input$colorByLines)){
      dendrogram <- IDBacApp::changeDendPartColor(dendrogram = dendrogram,
                                                  colorBy = input$colorByLines,
                                                  colorBlindPalette = colorBlindPalette(),
                                                  cutHeight = input$cutHeightLines,
                                                  chosenK = input$chosenKLines,
                                                  part = "branches")
    }
    if(!is.null(input$dendLineWidth)){
      dendrogram <- IDBacApp::changeDendPartSize(dendrogram = dendrogram,
                                                 dendPartSize = input$dendLineWidth,
                                                 part = "branches")
      
    }
    
    return(dendrogram)
  })
  
  
  

  
  output$hierOut <- renderPlot({

    if (!is.null(input$selectMetaColumn[[1]])){
      dendTrimmedLabels <- dendro()
      labs <- base::strtrim(labels(dendTrimmedLabels), 10)
      labels(dendTrimmedLabels) <- labs
      
      
      par(mar = c(5, 5, 5, plotWidth))
      plot(dendTrimmedLabels, horiz = TRUE)
      IDBacApp::runDendDots(rawDendrogram = dendro(),
                            trimdLabsDend = dendTrimmedLabels,
                            pool = pool,
                            columnID = input$selectMetaColumn,
                            colors = colorsChosen(),
                            text_shift = 1)
    }else{
      dendTrimmedLabels <- dendro()
      labs <- base::strtrim(labels(dendTrimmedLabels), 10)
      labels(dendTrimmedLabels) <- labs
      
      
      par(mar = c(5, 5, 5, plotWidth))
      plot(dendTrimmedLabels, horiz = TRUE)
    }
    
    if(!is.null(input$colorByLines)){
      if(input$colorByLines == "height"){
        abline(v= input$cutHeightLines, lty = 2)
        
      }
    }
    
    if(!is.null(input$colorByLabels)){
      if(input$colorByLabels == "height"){
        abline(v= input$cutHeightLabels, lty = 2)
      }
    }
    
  }, height=plotHeight)
  
  
  
}


