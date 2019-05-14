
# Small molecule page -------------------------------------------------



#nothing yet
#' smallMolDendrogram_UI
#'
#' @param id .
#'
#' @return .
#' @export
#'

smallMolDendrogram_UI <- function(id){
  ns <- shiny::NS(id)
  #empty
}






#' Title
#'
#' @param id shiny id
#'
#' @return shiny module ui
#' @export
#'
saveNetSVG <- function(id){
  ns <- shiny::NS(id)

  actionButton(ns("saveNetworkSvg"),
               label = "Save MAN as SVG", 
               icon = shiny::icon("download"))
  
}


# Brushable dend (protein) on small molecule page ---------------------------------------


#' manPageProtDend_Server
#'
#' @param input input 
#' @param output  output 
#' @param session  session 
#' @param dendrogram dendrogram 
#' @param colorByLines colorByLines 
#' @param cutHeightLines cutHeightLines 
#' @param colorByLabels colorByLabels 
#' @param cutHeightLabels cutHeightLabels 
#' @param plotHeight plotHeight 
#' @param plotWidth plotWidth 
#'
#' @return manPageProtDend_Server
#' @export
#'

manPageProtDend_Server <- function(input,
                                   output,
                                   session,
                                   dendrogram,
                                   colorByLines,
                                   cutHeightLines,
                                   colorByLabels,
                                   cutHeightLabels,
                                   plotHeight,
                                   plotWidth){
  
  output$hierOut <- renderPlot({
    req(!is.null(dendrogram$dendrogram))
    
    par(mar = c(5, 5, 5, plotWidth()))
    
    plot(dendrogram$dendrogram, horiz = TRUE)
    
    if (!is.null(colorByLines())) {
      if (colorByLines() == "height") {
        graphics::abline(v = cutHeightLines(), lty = 2)
        
      }
    }
    
    if (!is.null(colorByLabels())) {
      if (colorByLabels() == "height") {
        graphics::abline(v = cutHeightLabels(), lty = 2)
      }
    }
    
  }, height = plotHeight)
  
  
  
  
  return(reactive(input$plot_brush))
  
}


#' manPageProtDend_UI
#'
#' @param id namespace
#'
#' @return manPageProtDend_UI
#' @export
#'

manPageProtDend_UI <- function(id) {
  ns <- shiny::NS(id)
  
  plotOutput(ns("hierOut"),
             brush = ns("plot_brush"))
  
  
}




# MAN Creator -------------------------------------------------------------


#' smMANPlot_UI
#'
#' @param id .
#'
#' @return . 
#' @export
#'

smMANPlot_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(
      sigmajs::sigmajsOutput(ns("metaboliteAssociationNetwork"),
                             width = "100%",
                             height = "500px")
    )
  )
}





#' downloadSmNet_UI
#'
#' @param id shiny id
#'
#' @return shiny module ui
#' @export
#'

downloadSmNet_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    downloadButton(ns("downloadSmallMolNetworkData"),
                   label = "Download Current Network Data",
                   value = FALSE)
  )
}


#' Color MAN settings
#'
#' @param id shiny id
#'
#' @return shiny module ui
#' @export
#'
colorMANBy_UI <- function(id) {
  ns <- shiny::NS(id)
  selectInput(ns("colorMANBy"), 
              label = h3("Color MAN by:"), 
              choices = list("Color by Modularity" = "by_modularity",
                             "Color by Dendrogram Labels" = "by_dendLabels"), 
              selected = "by_modularity")
}

#' MAN_Server
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param subtractedMatrixBlank subtractedMatrixBlank 
#' @param sampleIDs sampleIDs
#' @param proteinDend protein dendrogram
#'
#' @return NA
#' @export
MAN_Server <- function(input,
                       output,
                       session,
                       subtractedMatrixBlank,
                       sampleIDs,
                       proteinDend){
  
  
  
  
  
  #----
  smallMolNetworkDataFrame <- reactive({
    req(MALDIquant::isMassPeaksList(subtractedMatrixBlank$maldiQuantPeaks))
    IDBacApp::smallMolDFtoNetwork(peakList = subtractedMatrixBlank$maldiQuantPeaks,
                                  sampleIDs = subtractedMatrixBlank$sampleIDs)
    
  })
  
  #----
  
  output$downloadSmallMolNetworkData <- downloadHandler(
    filename = function(){"SmallMolecule_Network.csv"
    },
    content = function(file){
      utils::write.csv(as.matrix(smallMolNetworkDataFrame()),
                       paste0(file, ".csv"),
                       row.names = FALSE)
    }
  )
  
  
  networkIgraph <- reactiveValues(graph = NULL)
  
  #This creates the network plot and calculations needed for such.
  #----
  observeEvent(c(smallMolNetworkDataFrame(), input$colorMANBy),{
    
    
    networkIgraph$graph <- IDBacApp::networkFromDF(smallMolNetworkDataFrame())
    
    req(igraph::is.igraph(networkIgraph$graph))
    len <- length(attributes(igraph::V(networkIgraph$graph))$names)
    req(len > 0)
    
    if (input$colorMANBy == "by_modularity") {
      networkIgraph$graph <- IDBacApp::modularityClustering(networkIgraph$graph)
      
    } else if (input$colorMANBy == "by_dendLabels") {
      
      
      dendColors <- cbind(id = labels(proteinDend$dendrogram),
                          color = dendextend::labels_col(proteinDend$dendrogram))
      
      igraph::V(networkIgraph$graph)$color <- rep("#000000FF", len)
      
      for (i in 1:len) {
        temp <- attributes(igraph::V(networkIgraph$graph))$names[i]
        col <- dendColors[,2][dendColors[,1] %in% temp]
        if (length(col) > 0) {
          igraph::V(networkIgraph$graph)$color[i] <- col
        }
      }
    }
    
    igraph::V(networkIgraph$graph)$label <- igraph::V(networkIgraph$graph)$name
    
    
    # Make sample nodes big
    sampleIndex <- which(igraph::V(networkIgraph$graph)$label %in% unique(smallMolNetworkDataFrame()$Source))
    
    igraph::V(networkIgraph$graph)$size <- rep(5, length(igraph::V(networkIgraph$graph)))
    
    igraph::V(networkIgraph$graph)$size[sampleIndex] <- 8
    
    # make m/z nodes smaller
    
  })
 
  
  
  output$metaboliteAssociationNetwork <- sigmajs::renderSigmajs({
    req(igraph::is.igraph(networkIgraph$graph))
    len <- length(attributes(igraph::V(networkIgraph$graph))$names)
    req(len > 0)
    
    igraph::E(networkIgraph$graph)$color <- "rgba(192, 192, 192, .8)"
    
    
    
    sigmajs::sigmajs() %>%
      sigmajs::sg_from_igraph(networkIgraph$graph) %>% 
      sigmajs::sg_settings(drawLabels = TRUE,
                           drawEdgeLabels = FALSE) %>% 
      #sigmajs::sg_force(edgeWeightInfluence = igraph::E(networkIgraph$graph)$Weight) %>% 
      sigmajs::sg_force_start() %>% # start
      sigmajs::sg_force_stop(5000) %>% # stop after 5 seconds
      sigmajs::sg_drag_nodes()
     
  })
  

  observeEvent(input$saveNetworkSvg, {
    
    sigmajs::sigmajsProxy(session$ns("metaboliteAssociationNetwork")) %>% 
      sigmajs::sg_export_svg_p(file = "MAN.svg", 
                               size="100%", 
                               labels = TRUE) 
  })
  
  
    
  
  
}









