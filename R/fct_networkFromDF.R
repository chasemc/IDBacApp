#' Network from data frame
#'
#' @param dataF dataframe
#'
#' @return igraph network
#' @export
#'
networkFromDF <- function(dataF){
  
  # Create igraph
  dataF <- igraph::graph_from_data_frame(dataF)
  
  #adjust wieght
  igraph::E(dataF)$Weight <- igraph::E(dataF)$Weight
  
  return(dataF)
  
}




#' Color network based on fastgreedy.community
#'
#' @param igraphNetwork igraph network
#' @param hexColors colors as hex
#'
#' @return colored igraph
#' @export
modularityClustering <- function(igraphNetwork,
                                 hexColors = IDBacApp::colorBlindPalette()[1:100]){
  
  clusters <- igraph::as.undirected(igraphNetwork)
  clusters <- igraph::fastgreedy.community(clusters)
  
  igraph::V(igraphNetwork)$color <- as.vector(hexColors)[clusters$membership]
  
  return(igraphNetwork)
}

