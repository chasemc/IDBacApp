
#' UI module for creating plotly tsne
#'
#' @param id namespace
#'
#' @return tsne UI
#' @export
#'
tsne_UI <- function(id){
  ns <- shiny::NS(id)
  tagList(
  shiny::numericInput(inputId = "tsnePerplexity",
                      label = "Perplexity",
                      value = 10,
                      min = 1,
                      max = 100,
                      step = 1,
                      width = NULL),
  shiny::numericInput(inputId = "tsneTheta",
                      label = "Theta",
                      value = .5,
                      min = 0,
                      max = 1,
                      step = 0.1,
                      width = NULL),
  shiny::numericInput(inputId = "tsneIterations",
                      label = "Iterations",
                      value = 1000,
                      min = 1,
                      max = 10000,
                      step = 1,
                      width = NULL),
  plotly::plotlyOutput(ns("tsnePlot"),
                       width = "100%")
  )
}



#' Plotly tsne Server using irlba
#'
#' @param input shiny
#' @param output shiny
#' @param session shiny
#' @param namedColors named vector, names are sample labels, hex colors are the vector
#' @param distanceMatrix distance matrix
#'
#' @return returns tsne results as reactive data frame
#' 
#' @export
#' 
tsne_Server <- function(input,
                        output,
                        session,
                        namedColors,
                        dataframe){ 
  
  
  
  tsneCalc <- reactive({
    
    validate(need(nrow(dataframe()) > 15, "t-SNE requires more samples"))
    
    validate(need(input$tsnePerplexity > 0, "Perplexity must be greater than 0"))
    validate(need(input$tsnePerplexity < 101, "Perplexity must be less than 101"))

    validate(need(input$tsneTheta > 0, "Theta must be greater than 0"))
    validate(need(input$tsneTheta < 1, "Theta must be less than 101"))
    
    validate(need(input$tsneIterations > 0, "Iterations must be greater than 0"))
    validate(need(input$tsneIterations < 10000, "Iterations must be less than 101"))
    
    IDBacApp::tsneCalculation(dataMatrix = dataframe(),
                              perplexity = input$tsnePerplexity,
                              theta = input$tsneTheta,
                              iterations = input$tsneIterations)
    
  })
  
  callModule(IDBacApp::popupPlot_server,
             "proteinTSNE",
             dataFrame = tsneCalc)
  
  
  
}