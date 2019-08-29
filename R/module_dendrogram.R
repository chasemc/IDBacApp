
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
    actionButton(ns("openDendots"), "Incorporate info about samples"),
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
  shinycssloaders::withSpinner(
    plotOutput(ns("hierOut"))
  )
  
}



#' Download newick hierarchical dendrogram
#'
#' @param id namespace
#'
#' @return NA
#'
#' @export
downloadHier <- function(id) {
  ns <- shiny::NS(id)
  
  downloadButton(ns("downloadHierarchical"),
                 "Save dendrogram as a Newick File")
  
}



#' Download svg hierarchical dendrogram
#'
#' @param id namespace
#'
#' @return NA
#' @export
#'
downloadSvg <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    downloadButton(ns("downloadSVG"),
                   "Save dendrogram as SVG image"),
    numericInput(ns("svgHeight"),
                 label = "SVG Height",
                 value = 15,
                 step	= 0.1),
    numericInput(ns("svgWidth"),
                 label = "SVG Width",
                 value = 10,
                 step	= 0.1),
    numericInput(ns("svgPointSize"),
                 label = "SVG Text Size",
                 value = 1,
                 step	= 0.1)
  )
  
  
}


#' Display samples removed from analysis
#'
#' @param id namespace
#' @param sampleIds sampleIds
#' 
#' @return shiny ui
#' @export
#'
displayMissingProteinUI <- function(id, sampleIds) {
  ns <- shiny::NS(id)
  uiOutput(ns("missingSamples"))
  
}






# Module Server -----------------------------------------------------------



#' Title
#'
#' @param input NA
#' @param output NA
#' @param session NA
#' @param dendrogram NA
#' @param pool NA
#' @param plotWidth NA
#' @param plotHeight NA
#' @param boots bootstraps
#' @param dendOrPhylo should the dendrogram labels hang?
#' @param emptyProtein empty protein sample IDs
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
                           plotHeight,
                           boots,
                           dendOrPhylo = "Dendrogram",
                           emptyProtein){
  
  
  
  output$missingSamples <- renderUI({
    shiny::p("The following samples were removed because they contained no peaks: ", 
             glue::glue_collapse(names(emptyProtein())[emptyProtein()], ", "))
  })
  
  
  
  observeEvent(input$closeDendDots, {
    output$absPaneldendDots <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  
  observeEvent(input$openDendots, ignoreInit = T ,ignoreNULL = T, {
    ns <- session$ns
    
    output$absPaneldendDots <- renderUI(
      
      IDBacApp::modDendDotsMod_WellPanel(session$ns)
      
    )
  })
  
  
  output$sampleFactorMapColors <- renderUI({
    column(7,
           IDBacApp::colorPicker(levs,
                                 session$ns)
           
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
                as.vector(a),
                width="100%")
    
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
    
    
    output$absPanelDendLabels <- renderUI(
      
      IDBacApp::modDendLabels_WellPanel(session$ns)
      
    )
  }
  
  )
  
  
  
  
  
  observeEvent(input$closeDendLines, {
    output$absPanelDendLines <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  observeEvent(input$openLineMod, ignoreInit = T ,ignoreNULL = T, {
    output$absPanelDendLines <- renderUI(
      IDBacApp::modDendLines_WellPanel(session$ns)
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
    
    
    shiny::validate(shiny::need(dendrogram$dendrogram, 
                                "Try selecting samples using the menu to the left."))
    
    par(mar = c(5,
                5,
                5,
                plotWidth()))
    
    
    IDBacApp::plotDendrogram(dendrogram = dendrogram,
                             dendOrPhylo = dendOrPhylo(),
                             selectMetaColumn = input$selectMetaColumn,
                             colorsChosen = colorsChosen(),
                             cutHeightLines = input$cutHeightLines,
                             colorByLines = input$colorByLines,
                             colorByLabels = input$colorByLabels,
                             removeDendDots = input$removeDendDots,
                             cutHeightLabels = input$cutHeightLabels,
                             boots = boots()$bootstraps,
                             pool = pool())
    
  }, height = plotHeight)
  
  
  
  
  
  # Download dendrogram as Newick
  #----
  output$downloadHierarchical <- downloadHandler(
    
    filename = function() {
      base::paste0(base::Sys.Date(), ".newick")
    },
    content = function(file) {
      req(!is.null(attributes(dendrogram$dendrogram)$members))
      
      ape::write.tree(ape::as.phylo(stats::as.hclust(dendrogram$dendrogram)), 
                      file = file)
    }
  )
  
  
  
  
  
  output$downloadSVG <- downloadHandler(
    filename = function(){
      base::paste0("dendrogram_",base::Sys.Date(),".svg")
      
    }, 
    content = function(file1){
      
      shiny::validate(shiny::need(dendrogram$dendrogram, "Try selecting samples using the menu to the left."))
      
      
      svglite::svglite(file1,
                       width = input$svgWidth,
                       height = input$svgHeight, 
                       bg = "white",
                       pointsize = input$svgPointSize,
                       standalone = TRUE)
      
      par(mar = c(1, 1, 1, plotWidth()))
      
      IDBacApp::plotDendrogram(dendrogram = dendrogram,
                               dendOrPhylo = dendOrPhylo(),
                               selectMetaColumn = input$selectMetaColumn,
                               colorsChosen = colorsChosen(),
                               cutHeightLines = input$cutHeightLines,
                               colorByLines = input$colorByLines,
                               colorByLabels = input$colorByLabels,
                               removeDendDots = input$removeDendDots,
                               cutHeightLabels = input$cutHeightLabels,
                               boots = boots()$bootstraps,
                               pool = pool())
      
      grDevices::dev.off()
      
      if (file.exists(paste0(file1, ".svg")))
        file.rename(paste0(file1, ".svg"), file1)
      
    })
  
  
  
  
  
  
  
  
  
  return(list(colorByLines = reactive(input$colorByLines),
              cutHeightLines = reactive(input$cutHeightLines),
              colorByLabels = reactive(input$colorByLabels),
              cutHeightLabels = reactive(input$cutHeightLabels)
  )
  )
  
  
}




#   -----------------------------------------------------------------------


#' modDendLabels_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modDendLabels_WellPanel <- function(ns) {
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
                           "Color by cutting at height" = "height"
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
}





#' modDendLines_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modDendLines_WellPanel <- function(ns){
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
                           "Color by cutting at height" = "height"
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
  
}



#' modDendDotsMod_WellPanel UI
#'
#' @param ns shiny namespace
#'
#' @return shiny ui
#' @export
#'
modDendDotsMod_WellPanel <- function(ns) {
  shiny::absolutePanel(
    class = "dendMod_WellPanel",
    top = "30%",
    right =  "0%",
    width = "30%",
    fixed = F,
    draggable = T,
    style = "z-index:1002;",
    shiny::wellPanel(class = "dendDots_WellPanel",
                     fluidRow(
                       fluidRow(shiny::actionButton(ns("closeDendDots"),
                                                    "Close"),
                                shiny::actionButton(ns("removeDendDots"),
                                                    "Clear"),
                         uiOutput(ns("proteDendDots")),
                         uiOutput(ns("sampleFactorMapColors")))
                     )
    ))
}










#' colorPicker for dend and dots
#'
#' @param levs levels (reactiveValue)
#' @param ns shiny namespace
#'
#' @return list of html for each level with colors chosen
#' @export
#'
colorPicker <-  function(levs, 
                         ns){
  lapply(seq_along(levs()),
         function(x){
           do.call((colourpicker::colourInput),
                   list(inputId = ns(paste0("factor-",
                                            make.unique(rep("dendDotsColors", length(levs())))[[x]])),
                        label = levs()[[x]],
                        value = "blue",
                        allowTransparent = T)
           )})
  
}