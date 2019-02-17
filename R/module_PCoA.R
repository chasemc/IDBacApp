
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
  
  
  
  calculation <- reactive({
    
    req(distanceMatrix()$distance)
    
    IDBacApp::pcoaCalculation(distanceMatrix = distanceMatrix()$distance)
    
  })
  
  callModule(IDBacApp::popupPlot_server,
             "proteinPCOA",
             dataFrame = calculation)
  
  
}