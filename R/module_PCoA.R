
#' UI module for creating plotly pcoa
#'
#' @param id namespace
#'
#' @return pcoa UI
#' @export
#'
pcoa_UI <- function(id){
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("pcoaPlot"),
                       width = "100%")
}



#' Plotly pcoa Server using irlba
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param namedColors named vector, names are sample labels, hex colors are the vector
#' @param distanceMatrix distance matrix
#'
#' @return returns pcoa results as reactive data frame
#' 
#' @export
#' 
pcoa_Server <- function(input,
                        output,
                        session,
                        namedColors,
                        distanceMatrix){ 
  
  
  
  pcoaCalc <- reactive({
    
    req(distanceMatrix()$distance)
    
    IDBacApp::pcoaCalculation(distanceMatrix = distanceMatrix()$distance)
    
  })
  
  
  
  output$pcoaPlot <- plotly::renderPlotly({
    req(nrow(pcoaCalc()) > 2,
        ncol(pcoaCalc()) > 2)
    
    if (is.null(namedColors())) {
      colorsToUse <- cbind.data.frame(fac = rep("#000000", nrow(pcoaCalc())), 
                                      pcoaCalc())
    } else {
      
      colorsToUse <- cbind.data.frame(fac = as.vector(namedColors()), 
                                      nam = (names(namedColors())))
      
      
      
      colorsToUse <- merge(pcoaCalc(),
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
                    text = ~nam) %>%
      plotly::layout(
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
  
  
  return(pcoaCalc)
}