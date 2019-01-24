
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
                                "Intensities" = FALSE))
  )

}



#' dendrogramCreator
#'
#' @param input shiny modules default
#' @param output shiny modules default
#' @param session shiny modules default
#' @param data matrix or dataframe, where columns are variables and rows are samples
#'
#' @return dendrogram
#' @export
#'

dendrogramCreator <- function(input,
                              output,
                              session,
                              data){


  data <- IDBacApp::distMatrix(data = data,
                              method = input$distanceMethod,
                              booled = input$booled)

  data <- stats::hclust(data,
                        method = input$clustering)
  

  return(stats::as.dendrogram(data))

}



