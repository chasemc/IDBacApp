#' Create Distance Matrix
#'
#' @param data matrix or dataframe, where columns are variables and rows are samples
#' @param method distance metric to use
#'
#' @return distance matrix
#' 

distMatrix <- function(data,
                       method){
   validate(need(nrow(data) > 2, "Need >2 samples to cluster")) 
  if (method == "cosine") {
    return(stats::as.dist(1 - coop::cosine(data)))
  }else{
    return(stats::dist(Matrix::t(data), method = method))
  }
}

