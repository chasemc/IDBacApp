
# Small molecule page -------------------------------------------------



#' MAN UI modules
#'
#' @param id namespace
#'
#' @return MAN UI
#' @export
#'

smallMolSettings_UI <- function(id) {
  ns <- shiny::NS(id)
# sidebar for now   
shiny::tagList(
                 radioButtons(ns("matrixSamplePresent"),
                              label = h5(strong("Do you have a matrix blank?")),
                              choices = list("Yes" = 1, 
                                             "No (Also Turns Off Matrix Subtraction)" = 2),
                              selected = 1),
                 numericInput(ns("percentPresenceSM"),
                              label = h5("In what percentage of replicates must a peak be present to be kept? (0-100%) (Experiment/Hypothesis dependent)"),
                              value = 70,
                              step = 10,
                              min = 0,
                              max = 100),
                 numericInput(ns("smSNR"),
                              label = h5(strong("Signal To Noise Cutoff")),
                              value = 4,
                              step = 0.5,
                              min = 1.5,
                              max = 100),
                 numericInput(ns("upperMassSM"),
                              label = h5(strong("Upper Mass Cutoff")),
                              value = 2000,
                              step = 20,
                              max = 3000),
                 numericInput(ns("lowerMassSM"),
                              label = h5(strong("Lower Mass Cutoff")),
                              value = 200,
                              step = 20,
                              min = 3000),
                 numericInput(ns("hclustHeightNetwork"),
                              label = h5(strong("Expand Tree")),
                              value = 750,
                              step = 50,
                              min = 100),
                 numericInput(ns("dendparmar2"),
                              label = h5(strong("Adjust right margin of dendrogram")),
                              value = 5),
                 downloadButton(ns("downloadSmallMolNetworkData"),
                                label = "Download Current Network Data",
                                value = FALSE),
                 br(),
                 p(strong("Hint 1:"),
                   "Use mouse to select parts of the tree and display the MAN of corresponding samples."),
                 p(strong("Hint 2:"),
                   "Use mouse to click & drag parts (nodes) of the MAN if it appears congested."),
                 br(),
                 
                 p(strong("Note 1:"), "For publication-quality networks click \"Download Current Network.\"
                   while selected- this saves a .csv file of the currently-displayed
                   network to the \"Saved_MANs\" folder in your working directory This can be easily imported into Gephi or Cytoscape.
                   While checked, any update of the network will overwrite this file. Also, an error saying: \"cannot open the connection\"
                   means this box is checked and the file is open in another program, either uncheck or close the file."),
                 br(),
                 h4("Suggestions for Reporting MAN Analysis:"),
                 uiOutput("manReport"),
                 br(),
                 h4("Suggestions for Reporting Protein Analysis"),
                 uiOutput("proteinReport2")
                 )
    
    
}
 

smallMolMAN_UI <- function(id){
  ns <- shiny::NS(id)
  
  networkD3::simpleNetworkOutput("metaboliteAssociationNetwork",
                                 width = "100%")

}


#nothing yet
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
        abline(v = cutHeightLines(), lty = 2)
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
  tagList(
    plotOutput(ns("hierOut"),
               brush = ns("plot_brush"))
  )
  
  
  
}




# MAN Creator -------------------------------------------------------------


smMANPlot_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    networkD3::simpleNetworkOutput(ns("metaboliteAssociationNetwork"),
                                   width = "100%")
  )
}

downloadSmNet_UI <- function(id) {
  ns <- shiny::NS(id)
shiny::tagList(
downloadButton(ns("downloadSmallMolNetworkData"),
               label = "Download Current Network Data",
               value = FALSE)
)
}


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
  
  
  output$metaboliteAssociationNetwork <- networkD3::renderSimpleNetwork({
    
    
    cbp <- as.vector(IDBacApp::colorBlindPalette()[1:100])
    
    
    YourColors <- paste0('d3.scaleOrdinal()
                         .domain([',paste0(shQuote(1:100), collapse = ", "),'])
                         .range([', paste0(shQuote(cbp), collapse = ", "),' ])')
    
    
    networkD3::forceNetwork(Links = calcNetwork()$z, 
                            Nodes = calcNetwork()$zz, 
                            Source = "source",
                            Nodesize = "biggerSampleNodes",
                            Target = "target",
                            NodeID = "name",
                            Group = "group",
                            opacity = 1,
                            opacityNoHover = 0.8, 
                            zoom = TRUE,
                            colourScale = networkD3::JS(YourColors))
    
  })
  
  


}












