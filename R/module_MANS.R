
# Small molecule page -------------------------------------------------




#' smallMolMAN_UI
#'
#' @param id . 
#'
#' @return .
#' @export
#'

smallMolMAN_UI <- function(id){
  ns <- shiny::NS(id)
  
  networkD3::simpleNetworkOutput("metaboliteAssociationNetwork",
                                 width = "100%")
  
}


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
        abline(v = cutHeightLines(), lty = 2)
        
      }
    }
    
    if (!is.null(colorByLabels())) {
      if (colorByLabels() == "height") {
        abline(v = cutHeightLabels(), lty = 2)
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
    networkD3::simpleNetworkOutput(ns("metaboliteAssociationNetwork"),
                                   width = "100%")
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
#'
#' @return NA
#' @export
#'
MAN_Server <- function(input,
                       output,
                       session,
                       subtractedMatrixBlank){
  
  
  
  
  
  #----
  smallMolNetworkDataFrame <- reactive({
    
    IDBacApp::smallMolDFtoNetwork(peakList = subtractedMatrixBlank())
    
  })
  
  #----
  
  output$downloadSmallMolNetworkData <- downloadHandler(
    filename = function(){"SmallMolecule_Network.csv"
    },
    content = function(file){
      write.csv(as.matrix(smallMolNetworkDataFrame()),
                file,
                row.names = FALSE)
    }
  )
  
  
  #This creates the network plot and calculations needed for such.
  #----
  calcNetwork <- reactive({
    manEnvironment <- new.env(parent = parent.frame())
    manEnvironment$sampleNodes <- character(length(subtractedMatrixBlank()))
    for (i in seq_along(subtractedMatrixBlank())) {
      manEnvironment$sampleNodes[i] <- subtractedMatrixBlank()[[i]]@metaData$Strain
    }
    
    a <- igraph::as.undirected(igraph::graph_from_data_frame(smallMolNetworkDataFrame()))
    a <- igraph::simplify(a)
    manEnvironment$wc <- igraph::fastgreedy.community(a)
    
    b <- networkD3::igraph_to_networkD3(a, group = (manEnvironment$wc$membership)) # zero indexed
    
    
    manEnvironment$z <- b$links
    manEnvironment$zz <- b$nodes
    manEnvironment$zz <- cbind(manEnvironment$zz, biggerSampleNodes = rep(1,times = length(manEnvironment$zz[,1])))
    manEnvironment$zz$biggerSampleNodes[which(manEnvironment$zz[,1] %in% manEnvironment$sampleNodes)] <- 50
    
    manEnvironment
    
  })
  
  
  output$metaboliteAssociationNetwork <- networkD3::renderForceNetwork({
    cbp <- as.vector(IDBacApp::colorBlindPalette()[1:100])
    
    
    YourColors <- paste0('d3.scaleOrdinal()
                         .domain([',paste0(shQuote(1:100), collapse = ", "),'])
                         .range([', paste0(shQuote(cbp), collapse = ", "),' ])')
    
   
    
    
    
    networkD3::forceNetwork(Links = calcNetwork()$z, 
                            Nodes = calcNetwork()$zz, 
                            Source = "source",
                            Value = smallMolNetworkDataFrame()$Weight,
                            Nodesize = "biggerSampleNodes",
                            Target = "target",
                            NodeID = "name",
                            Group = "group",
                            opacity = 1,
                            opacityNoHover = 0.8, 
                            zoom = TRUE,
                            colourScale = networkD3::JS(YourColors),
                            charge=-50, 
                            linkWidth = networkD3::JS("function(d) { return 1; }"),
                            linkDistance = networkD3::JS("function(d){return d.value * 10}"))
    
    
    
    
    
    
  })
  
  
  
  
}












