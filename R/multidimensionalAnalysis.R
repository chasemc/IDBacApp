#' Principle Components Analysis 
#' Given a data frame whose columns are variables and rows are samples, run Principle Components Analysis (PCA)
#' 
#' @param dataMatrix NA
#' @param logged  NA
#' @param scaled  NA
#' @param centered  NA
#' @param missing  NA
#'
#' @return a single trimmed and binned MALDIquant peak object
#' @export



pcaCalculation <- function(dataMatrix,
                           logged = TRUE,
                           scaled = TRUE,
                           centered = TRUE,
                           missing = .00001){
  
  names <- rownames(dataMatrix)
  # log10 if chosen
  if(logged){
    dataMatrix <- log10(dataMatrix)
  }
  
  # Check for infinite
  dataMatrix[is.infinite(dataMatrix)] <- missing
  # Check for NAs
  dataMatrix[is.na(dataMatrix)] <- missing
  
  #
  
  dataMatrix <- irlba::prcomp_irlba(dataMatrix,
                                    n = 3,
                                    retx = TRUE,
                                    scale = TRUE,
                                    centered = TRUE)

  dataMatrix <- dataMatrix$x[, 1:3]
  dataMatrix <- as.data.frame(dataMatrix)
  dataMatrix <- cbind.data.frame(names, 
                                 dataMatrix,
                                 stringsAsFactors = FALSE)
  colnames(dataMatrix) <- c("nam", "Dim1", "Dim2", "Dim3")
  return(dataMatrix)
  
}


#' t-SNE Analysis 
#' Given a data frame whose columns are variables and rows are samples, run t-SNE
#' 
#' @param dataMatrix NA
#' @param perplexity NA
#' @param theta NA
#' @param iterations NA
#'
#' @return a single trimmed and binned MALDIquant peak object
#' @export


tsneCalculation <- function(dataMatrix,
                            perplexity,
                            theta,
                            iterations){

  names <- rownames(dataMatrix)
  dataMatrix[is.na(dataMatrix)] <- 0
  dataMatrix <- Rtsne::Rtsne(dataMatrix,
                             pca = TRUE,
                             pca_center = TRUE,
                             pca_scale = TRUE,
                           #  partial_pca = TRUE,
                             normalize = TRUE,                           
                             dims = 3,
                             perplexity = perplexity,
                             theta = theta, 
                             max_iter = iterations)
  
  dataMatrix <- as.data.frame(dataMatrix$Y)
  dataMatrix <- cbind.data.frame(names,
                                 dataMatrix,
                                 stringsAsFactors = FALSE)
  colnames(dataMatrix) <- c("nam", "Dim1", "Dim2", "Dim3")
  return(dataMatrix)
}




#' Principle Coordinates Analysis 
#' Given a data frame whose columns are variables and rows are samples, run Principle Coordinates Analysis (PCoA)
#' 
#' @param distanceMatrix NA
#'
#' @return a single trimmed and binned MALDIquant peak object
#' @export



pcoaCalculation <- function(distanceMatrix){
  
  
  distanceMatrix <- as.data.frame(stats::cmdscale(distanceMatrix, k = 3))
  distanceMatrix <- distanceMatrix[,1:3]
  distanceMatrix <- cbind.data.frame(row.names(distanceMatrix),
                                     distanceMatrix,
                                     stringsAsFactors = FALSE)
  colnames(distanceMatrix) <- c("nam", "Dim1", "Dim2", "Dim3")
  return(distanceMatrix)
}




















