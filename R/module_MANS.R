
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
                             width = "100%")
    )
  )
}





#' downloadSmNet_UI
#'
#' @param id .
#'
#' @return .
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


#' MAN_Server
#'
#' @param input mod
#' @param output mod
#' @param session mod
#' @param subtractedMatrixBlank subtractedMatrixBlank 
#' @param sampleIDs sampleIDs
#'
#' @return NA
#' @export
#'
MAN_Server <- function(input,
                       output,
                       session,
                       subtractedMatrixBlank,
                       sampleIDs){
  
  
  
  
  
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
                       file,
                       row.names = FALSE)
    }
  )
  
  
  #This creates the network plot and calculations needed for such.
  #----
  calcNetwork <- reactive({
    zz1 <<- smallMolNetworkDataFrame()
    z1 <- igraph::graph_from_data_frame(smallMolNetworkDataFrame())
    z <- igraph::as.undirected(z1)
    clusters <- igraph::fastgreedy.community(z)
    
    igraph::V(z)$color <- as.vector(IDBacApp::colorBlindPalette()[1:100])[clusters$membership]
   igraph::V(z)$label <- igraph::V(z)$name
    
   igraph::E(z)$Weight <- igraph::E(z1)$Weight
    return(z)
  })
  
  
  output$metaboliteAssociationNetwork <- sigmajs::renderSigmajs({
    
    
    sigmajs::sigmajs() %>%
      sigmajs::sg_from_igraph(calcNetwork()) %>% 
      sigmajs::sg_settings(drawLabels = TRUE, drawEdgeLabels = FALSE) %>% 
      sigmajs::sg_force(edgeWeightInfluence = igraph::E(calcNetwork())$Weight*10) %>% 
      sigmajs::sg_force_start() %>% # start
      sigmajs::sg_force_stop(500) %>% # stop after 5 seconds
      sigmajs::sg_drag_nodes() 
    
    # sg_export_svg() %>% 
    # sg_button("force_start", "force", tag = htmltools::tags$button, position = "bottom")  
    # sg_button(
    #   "export_svg", # event to trigger
    #   class = "btn btn-default",
    #   "asdfsda",
    #   tag = htmltools::tags$button,
    #   tags$i(class = "fa fa-download")
    # )
    
    
    
    
  })
  
  
  
  
}












