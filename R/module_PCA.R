
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
#'     172-10     172-7     172-1    172-11 
#`    "#000000" "#000000" "#E69F00" "#E69F00" 
#'
#' @return
#'
manPageProtDend <- function(input,
                            output,
                            session,
                            dataframe,
                            namedColors){ 

  
  
  pcaCalc <- reactive({
    
  IDBacApp::pcaCalculation(dataMatrix = dataframe,
                               logged = TRUE,
                               scaled = TRUE,
                               centered = TRUE,
                               missing = 0.00001)
  })
  
  
  
  output$pcaPlot <- plotly::renderPlotly({
    
  
    
    colorsToUse <- dendextend::leaf_colors(coloredDend())
    
    if (any(is.na(as.vector(colorsToUse)))) {
      colorsToUse <-  dendextend::labels_colors(coloredDend())
    }
    
    colorsToUse <- cbind.data.frame(fac = as.vector(colorsToUse), 
                                    nam = (names(colorsToUse)))

    pcaDat <- merge(pcaCalc(),
                    colorsToUse, 
                    by = "nam")
    
    plot_ly(data = pcaDat,
            x = ~Dim1,
            y = ~Dim2,
            z = ~Dim3,
            type = "scatter3d",
            mode = "markers",
            marker = list(color = ~fac),
            hoverinfo = 'text',
            text = ~nam) %>%
      layout(
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = " "
        ),
        zaxis = list(
          title = ""
        ))
})
}