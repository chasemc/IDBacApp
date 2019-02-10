#' Create Distance Matrix
#'
#' @param data matrix or dataframe, where columns are variables and rows are samples
#' @param method distance metric to use
#' @param booled whether all values >0 should be set to one
#'
#' @return distance matrix
#' @export

distMatrix <- function(data,
                       method,
                       booled){
  req(nrow(data) > 2)
  data <- base::as.matrix(data)
  # Change empty to 0
  data[base::is.na(data)] <- 0
  data[base::is.null(data)] <- 0
  
  if(booled == "TRUE"){
    data[data > 0] <- 1
  }
  
  if(method == "cosine"){
    return(stats::as.dist(1 - coop::tcosine(data)))
  }else{
    return(stats::dist(data, method = method))
  }
}

