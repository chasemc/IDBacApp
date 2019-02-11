
#' dendrogramCreatorUI
#'
#' @param id namespace id
#'
#' @return ui to choose the algorithm for creating the dendrogram
#' @export
#'

dendrogramCreatorUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::selectInput(ns("distanceMethod"),
                       label = shiny::h5(shiny::strong("Distance Algorithm")),
                       choices = list("cosine" = "cosine",
                                      "euclidean" = "euclidean",
                                      "maximum" = "maximum",
                                      "manhattan" = "manhattan",
                                      "canberra" = "canberra",
                                      "binary" = "binary",
                                      "minkowski"= "minkowski"),
                       selected = "cosine"),
    
    shiny::selectInput(ns("clustering"),
                       label = shiny::h5(shiny::strong("Clustering Algorithm")),
                       choices = list("ward.D" = "ward.D",
                                      "ward.D2" = "ward.D2",
                                      "single" = "single",
                                      "complete" = "complete",
                                      "average (UPGMA)" = "average",
                                      "mcquitty (WPGMA)" = "mcquitty",
                                      "median (WPGMC)" = "median",
                                      "centroid (UPGMC)" = "centroid"),
                       selected = "average"),
    
    shiny::radioButtons(ns("booled"),
                        label = shiny::h5(shiny::strong("Include peak intensities, or use presence/absence?")),
                        choices = list("Presence/Absence" = TRUE,
                                       "Intensities" = FALSE)),
    shiny::numericInput(ns("bootstraps"),
                        label = "Bootstraps",
                        value = "",
                        min = 1,
                        max = 1000)
  )
  
}



#' dendrogramCreator
#'
#' @param input shiny modules default
#' @param output shiny modules default
#' @param proteinMatrix proteinMatrix , rows are samples, cols are peak intensities
#' @param session shiny modules default
#'
#' @return dendrogram
#' @export
#'

dendrogramCreator <- function(input,
                              output,
                              session,
                              proteinMatrix){
  
  
  pMatrixReactive <- reactive({
    
    req(nrow(proteinMatrix() > 2))
    
    
    # Remove if row is all NA (no peaks left)
    dend <- proteinMatrix()[rowSums(is.na(proteinMatrix())) > 0, ]
    
    
    createHclustObject <- function(x){
      x <- IDBacApp::distMatrix(data = x,
                                method = input$distanceMethod,
                                booled = input$booled)
      
      stats::hclust(x,
                    method = input$clustering)
    }
    bootstraps <- ""
    if (is.numeric(input$bootstraps)) {
      if ((input$bootstraps > 1) & (input$bootstraps < 1000)) {
        
        bootstraps <- IDBacApp::bootstrap(dend,
                                          fun = createHclustObject,
                                          n = input$bootstraps)
        
        
      }
    }
    
    
    dend <- createHclustObject(dend)
    
    dend <- stats::as.dendrogram(dend)
    
    
    return(list(dendrogram = dend,
                bootstraps = bootstraps))
  })
  
  return(pMatrixReactive)
  
}



