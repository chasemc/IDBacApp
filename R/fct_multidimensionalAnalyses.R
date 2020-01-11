#' Principle Components Analysis
#' 
#' @param dataMatrix data as matrix, columns are variables and rows are samples
#' @param logged  should the matrix by log10 normalized?
#' @param scaled  NA 
#' @param centered  NA
#' @param missing  Missing values should be replaced with...
#'
#' @return a single trimmed and binned MALDIquant peak object
#' @export



pcaCalculation <- function(dataMatrix,
                           logged = FALSE,
                           scaled = FALSE,
                           centered = FALSE,
                           missing = 0L){
  validate(need(nrow(dataMatrix) > 3, "Select more samples for PCA"))
  validate(need(ncol(dataMatrix) > 1, "Only 1 peak found between all samples"))
  
  names <- rownames(dataMatrix)
  # log10 if chosen
  if (logged) {
    dataMatrix <- log10(dataMatrix)
  }
  
  if (scaled) {
    dataMatrix <- scale(dataMatrix, 
                        scale = scaled,
                        center = centered)
  }
  
  # Check for infinite
  dataMatrix[is.infinite(dataMatrix)] <- missing
  # Check for NAs
  dataMatrix[is.na(dataMatrix)] <- missing
  dataMatrix <- irlba::prcomp_irlba(dataMatrix,
                                    n = 3,
                                    fastpath=FALSE)

  dataMatrix <- dataMatrix$x[, 1:3]
  dataMatrix <- as.data.frame(dataMatrix)
  dataMatrix <- cbind.data.frame(names, 
                                 dataMatrix,
                                 stringsAsFactors = FALSE)
  colnames(dataMatrix) <- c("nam", "Dim1", "Dim2", "Dim3")
  return(dataMatrix)
  
}


#' t-SNE Analysis 
#' 
#' @param dataMatrix data as matrix, columns are variables and rows are samples
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
  
  validate(need(nrow(dataMatrix) > 1, "Need more samples for t-SNE"))
  validate(need(ncol(dataMatrix) > 5, "Only 1-peak found between all samples"))
  names <- rownames(dataMatrix)
  dataMatrix[is.na(dataMatrix)] <- 0
  
  if (nrow(dataMatrix) > 50) {
    
    # dataMatrix <- scale(dataMatrix, 
    #                     scale = TRUE, 
    #                     center = TRUE)
    # 
    # # Check for infinite
    # dataMatrix[is.infinite(dataMatrix)] <- missing
    # # Check for NAs
    # dataMatrix[is.na(dataMatrix)] <- missing
    # 
    # 
    # dataMatrix <- irlba::prcomp_irlba(dataMatrix,
    #                                   n = 50)
    dataMatrix <- dataMatrix[ , -which(colSums(dataMatrix) == 0)]
    
    dataMatrix <- Rtsne::Rtsne(dataMatrix,
                               pca = T,
                               pca_center = T,
                               pca_scale = F,
                               partial_pca = T,
                               normalize = TRUE,                           
                               dims = 3,
                               perplexity = perplexity,
                               theta = theta, 
                               max_iter = iterations)
  } else {
    dataMatrix <- Rtsne::Rtsne(dataMatrix,
                               pca = TRUE,
                               pca_center = TRUE,
                               pca_scale = F,
                               partial_pca = FALSE,
                               normalize = TRUE,                           
                               dims = 3,
                               perplexity = perplexity,
                               theta = theta, 
                               max_iter = iterations)
  }
  
  
 
  
  dataMatrix <- as.data.frame(dataMatrix$Y)
  dataMatrix <- cbind.data.frame(names,
                                 dataMatrix,
                                 stringsAsFactors = FALSE)
  colnames(dataMatrix) <- c("nam", "Dim1", "Dim2", "Dim3")
  return(dataMatrix)
}




#' Principle Coordinates Analysis 
#' 
#' @param distanceMatrix distance matrix 
#'
#' @return 3D pcoa data framw
#' @export



pcoaCalculation <- function(distanceMatrix){
  
 validate(need(nrow(as.matrix(distanceMatrix)) > 3, "Select more samples for PCoA"))
  
  distanceMatrix <- as.data.frame(stats::cmdscale(distanceMatrix, k = 3))
  distanceMatrix <- distanceMatrix[,1:3]
  distanceMatrix <- cbind.data.frame(row.names(distanceMatrix),
                                     distanceMatrix,
                                     stringsAsFactors = FALSE)
  colnames(distanceMatrix) <- c("nam", "Dim1", "Dim2", "Dim3")
  return(distanceMatrix)
}

