
#' UI module for creating absolute panel popup
#'
#' @param id namespace
#' @param name name
#'
#' @return PCA UI
#' @export
#'
pca2_UI <- function(id){
  ns <- shiny::NS(id)
  
  IDBacApp::popupPlot_UI(ns("output2"),"PCA")
  
}


#' Plotly PCA Server using irlba
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param dataframe dataframe that PCA will be performed on
#' @param namedColors named vector, names are sample labels, hex colors are the vector
#'
#' @return returns PCA results as reactive data frame
#' 
#' @export
#' 
pca_Server <- function(input,
                       output,
                       session,
                       dataframe,
                       namedColors){ 
  
  
  
  calculation <- reactive({
    
    IDBacApp::pcaCalculation(dataMatrix = dataframe(),
                             logged = TRUE,
                             scaled = TRUE,
                             centered = TRUE,
                             missing = 0.00001)
  })
  
  
  callModule(IDBacApp::popupPlot_server,
             "output2",
             dataFrame = calculation,
             namedColors = namedColors)
  
  
  
  
}