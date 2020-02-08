





#' dendrogramActionsUI
#'
#' @param id namespace id
#'
#' @return shiny UImodule
#' @export
#'
dendrogramActionsUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("openDendots"), "Incorporate info about samples"),
    actionButton(ns("openLineMod"), "Click to modify lines"),
    actionButton(ns("openLabelMod"), "Click to modify labels"),
    actionButton(ns("openAppendDendLabels"), "Click to append to  labels")
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
  
  
  
  
  
  
  
  
  
  
  
  output$sampleFactorMapColors <- renderUI({
    column(7,
           colorPicker(levs,
                                 session$ns)
           
    )
  })
  
  
  levs <- reactive({
    req(input$selectMetaColumn)
    conn <- pool::poolCheckout(pool())
    dendLabs <- labels(dendrogram$dendrogram)
    query <- DBI::dbSendStatement("SELECT *
                                  FROM metadata
                                  WHERE `strain_id` = ?",
                                  con=conn)
    DBI::dbBind(query, list(dendLabs))
    selectedMeta <- DBI::dbFetch(query)
    
    DBI::dbClearResult(query)
    pool::poolReturn(conn)
    selectedMeta <- selectedMeta[ , colnames(selectedMeta) %in% input$selectMetaColumn]
    selectedMeta[is.na(selectedMeta)] <- "Missing MetaData"
    
    return(unique(selectedMeta))
  })
  
  
  output$metaColumns <- renderUI({
    ns <- session$ns 
    a <- DBI::dbListFields(pool(), "metadata")
    a <- a[-which(a == "strain_id")]
    
    selectInput(ns("selectMetaColumn"),
                "Select Category",
                as.vector(a),
                width="100%")
    
  })
  
  output$appendmetaColumns <- renderUI({
    ns <- session$ns 
    a <- DBI::dbListFields(pool(), "metadata")
    a <- a[-which(a == "strain_id")]
    
    selectInput(ns("appendselectMetaColumn"),
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
  
  # -------------------------------------------------------------------------
  
  observeEvent(input$closeDendDots, {
    output$absPaneldendDots <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  
  
  observeEvent(input$openDendots, ignoreInit = T ,ignoreNULL = T, {
    ns <- session$ns
    
    output$absPaneldendDots <- renderUI(
      
      modDendDotsMod_WellPanel(session$ns)
      
    )
  })
  
  # -------------------------------------------------------------------------
  
  observeEvent(input$closeDendLabels, {
    output$absPanelDendLabels <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  
  observeEvent(input$openLabelMod, ignoreInit = T ,ignoreNULL = T, {
    output$absPanelDendLabels <- renderUI(
      modDendLabels_WellPanel(session$ns)
    )
  })
  
  
  
  # -------------------------------------------------------------------------
  
  
  
  
  observeEvent(input$closeDendLines, {
    output$absPanelDendLines <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  observeEvent(input$openLineMod, ignoreInit = T ,ignoreNULL = T, {
    output$absPanelDendLines <- renderUI(
      modDendLines_WellPanel(session$ns)
    )
  })
  
  
  # -------------------------------------------------------------------------
  
  
  
  observeEvent(input$closeModAppendDendLabels, {
    output$absPanelmodAppendDendLabels <- renderUI({
      # Intentionally Blank
    })
  })  
  
  
  observeEvent(input$openAppendDendLabels, ignoreInit = T ,ignoreNULL = T, {
    output$absPanelmodAppendDendLabels <- renderUI(
      modAppendDendLabels_WellPanel(session$ns)
    )
  })
  
  
  
  # -------------------------------------------------------------------------
  
  
  
  observeEvent(c(input$colorByLabels,input$cutHeightLabels,input$chosenKLabels), {
    
    
    dendrogram$dendrogram <- changeDendPartColor(dendrogram = dendrogram$dendrogram,
                                                           colorBy = input$colorByLabels,
                                                           colorBlindPalette = colorBlindPalette(),
                                                           cutHeight = input$cutHeightLabels,
                                                           chosenK = input$chosenKLabels,
                                                           part = "labels")
    
  })
  
  observeEvent(c(input$colorByLines,input$cutHeightLines,input$chosenKLines), {
    
    dendrogram$dendrogram <- changeDendPartColor(dendrogram = dendrogram$dendrogram,
                                                           colorBy = input$colorByLines,
                                                           colorBlindPalette = colorBlindPalette(),
                                                           cutHeight = input$cutHeightLines,
                                                           chosenK = input$chosenKLines,
                                                           part = "branches")
  })
  
  observeEvent(input$dendLabelSize, {
    
    dendrogram$dendrogram <- changeDendPartSize(dendrogram = dendrogram$dendrogram,
                                                          dendPartSize = input$dendLabelSize,
                                                          part = "labels")
  })
  
  
  observeEvent(input$dendLineWidth, {
    
    dendrogram$dendrogram <- changeDendPartSize(dendrogram =  dendrogram$dendrogram,
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
    
    
    
    plotDendrogram(dendrogram = dendrogram,
                             dendOrPhylo = dendOrPhylo(),
                             selectMetaColumn = input$selectMetaColumn,
                             appendDendLabels = input$appendselectMetaColumn,
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
      
      plotDendrogram(dendrogram = dendrogram,
                               dendOrPhylo = dendOrPhylo(),
                               selectMetaColumn = input$selectMetaColumn,
                               appendDendLabels = input$appendselectMetaColumn,
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









