

#' dendrogramCreator
#'
#' @param bootstraps integer number of bootstraps
#' @param distanceMethod distance method to use
#' @param clusteringMethod clustering method to use
#' @param proteinMatrix proteinMatrix , rows are samples, cols are peak intensities
#'
#' @return dendrogram
#' @export
#'

idbac_dendrogram_creator <- function(bootstraps = 0L,
                                     distanceMethod,
                                     clusteringMethod,
                                     proteinMatrix){
  
  
  createHclustObject <- function(x){
    x <- IDBacApp::distMatrix(data = x,
                              method = distanceMethod)
    
    stats::hclust(x,
                  method = clusteringMethod)
  }
  
  if (is.numeric(bootstraps)) {
    if ((bootstraps > 1) & (bootstraps < 1000)) {
      
      bootstraps <- IDBacApp::bootstrap(proteinMatrix,
                                        fun = createHclustObject,
                                        n = bootstraps)
      
      
    }
  }
  
  distance_matrix <- IDBacApp::distMatrix(data = proteinMatrix,
                                          method = distanceMethod)
  
  dend <- stats::hclust(distance_matrix,
                        method = clusteringMethod)
  
  dend <- stats::as.dendrogram(dend)
  
  
  return(list(dendrogram = dend,
              bootstraps = bootstraps,
              distance = distance_matrix))
  
  
  
}


