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
                           logged = FALSE,
                           scaled = TRUE,
                           centered = TRUE,
                           missing = 0){
  validate(need(nrow(dataMatrix) > 3, "Select more samples for PCA"))
  validate(need(ncol(dataMatrix) > 1, "Only 1 peak found between all samples"))
  
  names <- rownames(dataMatrix)
  # log10 if chosen
  if (logged) {
    dataMatrix <- log10(dataMatrix)
  }
  
#  dataMatrix <- scale(dataMatrix, scale = scaled, center = centered)
  
  
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
#' Given a data frame whose columns are variables and rows are samples, run Principle Coordinates Analysis (PCoA)
#' 
#' @param distanceMatrix NA
#'
#' @return a single trimmed and binned MALDIquant peak object
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

