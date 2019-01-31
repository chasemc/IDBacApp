
#' UI module for creating plotly PCA
#'
#' @param id namespace
#'
#' @return PCA UI
#' @export
#'
pca_UI <- function(id){
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("pcaPlot"),
                       width = "100%")
}



#' PCA Server
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param dataframe dataframe that PCA will be performed on
#' @param namedColors named vector, names are sample labels, hex colors are the vector
#'
#' @return returns PCA results as reactive data frame
#'
pca_Server <- function(input,
                       output,
                       session,
                       dataframe,
                       namedColors){ 
  
  
  
  pcaCalc <- reactive({
    
    IDBacApp::pcaCalculation(dataMatrix = dataframe(),
                             logged = TRUE,
                             scaled = TRUE,
                             centered = TRUE,
                             missing = 0.00001)
  })
  
  
  
  output$pcaPlot <- plotly::renderPlotly({
    req(nrow(pcaCalc()) > 2,
        ncol(pcaCalc()) > 2)
    
    if (is.null(namedColors())) {
      colorsToUse <- cbind.data.frame(fac = rep("#000000", nrow(pcaCalc())), 
                                      pcaCalc())
    } else {
    
    colorsToUse <- cbind.data.frame(fac = as.vector(namedColors()), 
                                    nam = (names(namedColors())))
    
    
    
    colorsToUse <- merge(pcaCalc(),
                         colorsToUse, 
                         by = "nam")
    }
    
  plotly::plot_ly(data = colorsToUse,
            x = ~Dim1,
            y = ~Dim2,
            z = ~Dim3,
            type = "scatter3d",
            mode = "markers",
            marker = list(color = ~fac),
            hoverinfo = 'text',
            text = ~nam) 
  
  # %>%
  #     layout(
  #       xaxis = list(
  #         title = ""
  #       ),
  #       yaxis = list(
  #         title = " "
  #       ),
  #       zaxis = list(
  #         title = ""
  #       ))
  })
  
  
  return(pcaCalc)
}