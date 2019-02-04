
#' Modify (color/size) dendrogram lines
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

#' Modify (color/size) dendrogram Labels
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
#' @param pool NA
#' @param plotWidth NA
#' @param plotHeight NA
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
        class = "dendMod_WellPanel",
        bottom = "50%",
        right =  "0%",
        width = "20%",
        fixed = T,
        draggable = TRUE,
        style = "z-index:1002;",
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
    column(7,
           lapply(seq_along(levs()),
                  function(x){
                    ns <- session$ns 
                    
                    do.call((colourpicker::colourInput),
                            list(inputId = ns(paste0("factor-",
                                                     make.unique(rep("dendDotsColors", length(levs())))[[x]])),
                                 label = levs()[[x]],
                                 value = "blue",
                                 allowTransparent = T))})
           
    )
  })
  
  
  levs <- reactive({
    req(input$selectMetaColumn)
    conn <- pool::poolCheckout(pool())
    dendLabs <- labels(dendrogram$dendrogram)
    query <- DBI::dbSendStatement("SELECT *
                                  FROM metaData
                                  WHERE `Strain_ID` = ?",
                                  con=conn)
    DBI::dbBind(query, list(dendLabs))
    selectedMeta <- DBI::dbFetch(query)
    
    DBI::dbClearResult(query)
    pool::poolReturn(conn)
    selectedMeta <- selectedMeta[ , colnames(selectedMeta) %in% input$selectMetaColumn]
    selectedMeta[is.na(selectedMeta)] <- "Missing MetaData"

    return(unique(selectedMeta))
  })
  
  
  output$proteDendDots <- renderUI({
    ns <- session$ns 
    a <- DBI::dbListFields(pool(), "metaData")
    a <- a[-which(a == "Strain_ID")]
    
    selectInput(ns("selectMetaColumn"),
                "Select Category",
                as.vector(a))
    
  })
  
  
  
  colorsChosen <- reactive({
    sapply(seq_along(levs()),
           function(x){ 
             input[[paste0("factor-",
                           make.unique(rep("dendDotsColors", length(levs())))[[x]])]]
             
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
        class = "dendMod_WellPanel",
        bottom = "0%",
        right = "0%",
        width = "20%",
        fixed = TRUE,
        draggable = TRUE,
        style = "z-index:1002;",
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
        class = "dendMod_WellPanel",
        bottom = "50%",
        right =  "0%",
        width = "20%",
        fixed = TRUE,
        draggable = TRUE,
        style = "z-index:1002;",
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
  
  
  
  observeEvent(c(input$colorByLabels,input$cutHeightLabels,input$chosenKLabels), {
  
  dendrogram$dendrogram <- IDBacApp::changeDendPartColor(dendrogram = dendrogram$dendrogram,
                                                         colorBy = input$colorByLabels,
                                                         colorBlindPalette = colorBlindPalette(),
                                                         cutHeight = input$cutHeightLabels,
                                                         chosenK = input$chosenKLabels,
                                                         part = "labels")
  
  })
  
  observeEvent(c(input$colorByLines,input$cutHeightLines,input$chosenKLines), {
    
    dendrogram$dendrogram <- IDBacApp::changeDendPartColor(dendrogram = dendrogram$dendrogram,
                                                           colorBy = input$colorByLines,
                                                           colorBlindPalette = colorBlindPalette(),
                                                           cutHeight = input$cutHeightLines,
                                                           chosenK = input$chosenKLines,
                                                           part = "branches")
  })
  
  observeEvent(input$dendLabelSize, {

    dendrogram$dendrogram <- IDBacApp::changeDendPartSize(dendrogram = dendrogram$dendrogram,
                                                          dendPartSize = input$dendLabelSize,
                                                          part = "labels")
  })
  
  
  observeEvent(input$dendLineWidth, {
    
    dendrogram$dendrogram <- IDBacApp::changeDendPartSize(dendrogram =  dendrogram$dendrogram,
                                               dendPartSize = input$dendLineWidth,
                                               part = "branches")
    
  })
 
 
 
  
    
  
  
  
  
  
  output$hierOut <- renderPlot({

    shiny::validate(shiny::need(dendrogram$dendrogram, "Try selecting samples using the menu to the left."))
    
    
    par(mar = c(5, 5, 5, plotWidth()))
   plot(dendrogram$dendrogram, horiz = T)
     if (!is.null(input$selectMetaColumn[[1]])) {
      
      if (input$closeDendDots == 1) {
        
      } else {
        
        trimdLabsDend <- dendrogram$dendrogram
        labels(trimdLabsDend) <- strtrim(labels(trimdLabsDend), 20)
        IDBacApp::runDendDots(rawDendrogram =  dendrogram$dendrogram,
                              trimdLabsDend = trimdLabsDend,
                              pool = pool(),
                              columnID = input$selectMetaColumn,
                              colors = colorsChosen(),
                              text_shift = 1)
      }
    }
    

    if (!is.null(input$colorByLines)) {
      if (input$colorByLines == "height") {
        abline(v = input$cutHeightLines, lty = 2)
        
      }
    }
    
    if (!is.null(input$colorByLabels)) {
      if (input$colorByLabels == "height") {
        abline(v = input$cutHeightLines, lty = 2)
      }
    }
    
  }, height = plotHeight)
  
  
  return(list(colorByLines = reactive(input$colorByLines),
              cutHeightLines = reactive(input$cutHeightLines),
              colorByLabels = reactive(input$colorByLabels),
              cutHeightLabels = reactive(input$cutHeightLabels)
              )
  )
  
  
}



