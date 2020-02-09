
#' dendrogramCreatorUI
#'
#' @param id namespace id
#'
#' @return ui to choose the algorithm for creating the dendrogram
#' 
#'

dendrogramCreatorUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    div(class = "tooltippy", "Distance Algorithm", 
        span(class = "tooltippytext", 
             p("Method for measuring the similarity of peaks between spectra.")
        )
    ),
    shiny::selectInput(ns("distanceMethod"),
                       label = NULL,
                       choices = list("cosine" = "cosine",
                                      "euclidean" = "euclidean",
                                      "maximum" = "maximum",
                                      "manhattan" = "manhattan",
                                      "canberra" = "canberra",
                                      "binary" = "binary",
                                      "minkowski" = "minkowski"),
                       selected = "cosine",
                       width = "50%"),
    div(class = "tooltippy", "Clustering Algorithm", 
        span(class = "tooltippytext", 
             p("Method for creating the dendrogram from a similarity/distance matrix.")
        )
    ),
    shiny::selectInput(ns("clustering"),
                       label = NULL,
                       choices = list("ward.D" = "ward.D",
                                      "ward.D2" = "ward.D2",
                                      "single" = "single",
                                      "complete" = "complete",
                                      "average (UPGMA)" = "average",
                                      "mcquitty (WPGMA)" = "mcquitty",
                                      "median (WPGMC)" = "median",
                                      "centroid (UPGMC)" = "centroid"),
                       selected = "average",
                       width = "50%"),
    fluidRow(
      div(style = "text-align: left;",
          shiny::radioButtons(ns("booled"),
                              label =  p("Include peak intensities, or use presence/absence?"),
                              choices = list("Presence/Absence" = TRUE,
                                             "Intensities" = FALSE),
                              selected = FALSE)
      )
    ),
    # shiny::numericInput(ns("ppm"),
    #                     label = shiny::h5(shiny::strong("Select ppm tolerance for peak binning")),
    #                     min =  50,
    #                     max = 10000,
    #                     value = 300),
    fluidRow(
      shiny::numericInput(ns("bootstraps"),
                          label = "Bootstraps",
                          value = "",
                          min = 1,
                          max = 1000)
    )
  )
  
}




#' Title
#'
#' @param input sd
#' @param output sd
#' @param session sd
#' @param proteinMatrix sd
#'
#' @return sd
#' 
dendrogramCreator <- function(input,
                              output,
                              session,
                              proteinMatrix){
  
  pMatrixReactive <- reactive({
  
    #require more than two samples
    req(ncol(proteinMatrix() > 2))

    idbac_dendrogram_creator(bootstraps = 0L,
                      distanceMethod = input$distanceMethod,
                      clusteringMethod = input$clustering,
                      proteinMatrix = proteinMatrix())
    
  })
  
}