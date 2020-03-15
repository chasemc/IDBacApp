
#' scaler so smallest mass differnce is 1
#'
#' @param mass numeric 
#' @param ppm numeric
#'
#' @return numeric
#'
.scale_ppm <- function(mass,
                       ppm){
  1L / ((ppm / 1e6) * mass)
}


#' High-dimensional representation of mass peaks
#'
#' @param massStart beginning of mass range (as of now, must be smaller than smallest mass)
#' @param massEnd end of mass range (as of now, must be smaller than smallest mass)
#' @param ppm ppm tolerance
#' @param massList list of mass vectors (eg list(1:10, 1:10))
#' @param intensityList list of intesity vectors (eg list(1:10, 1:10))
#'
#' @return matrix where rows are samples and columns are variables (m/z preojections)
#' 
#' @importFrom stats dnorm
#'
#' @examples
#' \dontrun{
#' masses <- list(Sample_A = c(5000,6000,7000), 
#' Sample_B = c(5000,6010,7005), Sample_C = c(5000,6010,7005))  
#' intensities <- list(Sample_A = rep(1, 3), 
#' Sample_B = rep(1, 3),
#'  Sample_C = rep(1, 3))
#' zx <- binnR(massStart = 3000,
#'             massEnd = 15000,
#'             ppm = 500,
#'             massList = masses,
#'             intensityList = intensities)
#'             }
createFuzzyVector <- function(massStart,
                              massEnd,
                              ppm,
                              massList,
                              intensityList){
  
  if (!all(lengths(massList) > 0)) {
    stop(paste0(sum(!lengths(massList) > 0), "Empty massList found in createFuzzyVector()"))
  }
  
  scaler <- .scale_ppm(ppm = ppm, 
                       mass = massStart)
  
  # smallest start mass
  z1 <- floor((massStart * scaler) - ((massStart * scaler) * ppm) / 1e6)
  vecLength <-  ceiling((massEnd * scaler) + ((massEnd * scaler) * ppm) / 1e6)
  
  # mm returns a list of lists. each list element contains a list of length 3:
  # 1- centroid - ppm 
  # 2- centroid
  # 3- centroid + ppm
  mm <- lapply(massList, function(x){
    # scale and shift to begin at 1
    #z1 <- as.integer(round((x * scaler) - toSub))
    z1 <- as.integer(round((x * scaler)))
    z2 <- as.integer(round(((x * scaler) * ppm) / 1e6))    
    list(z1 - z2,
         z1,
         z1 + z2)
  })
  
  # Create a distribution of "intensity" across each ppm range of each peak
  # loop across all samples (spectra) 
  w <- lapply(seq_along(mm), function(i){
    
    # For each centroid, create a sequence of integers from
    # (centroid mass - ppm) to (centroid mass + ppm)
    z <- mapply(function(x, y) {x:y},
                mm[[i]][[1]],
                mm[[i]][[3]], SIMPLIFY = F)
    
    # For each centroid we now have a sequence of integers representing uncertainty of the centroid
    # model normal distribution density across each centroid's uncertainty integer vector
    z2 <- mapply(
      function(x,y,z){
        round(dnorm(x,
                    y,
                    (z - y) / 4),
              digits = 4)
      },
      z,
      mm[[i]][[2]],
      mm[[i]][[3]],
      SIMPLIFY = FALSE)
    
    # Multiple each centroid's intensity against its above calculated distribution
    #z3 <-  mapply(function(x, y) x * y, z2, intensityList[[i]])
    z3 <-  mapply(function(x, y) x * y,
                  z2,
                  1000,
                  SIMPLIFY = FALSE)
    
    if (!class(z3) == "list") {
      z3 <- list(z3)
    }
    
    z <- unlist(z)
    
    data.table::data.table(index = unique(z),
                           intensity = as.numeric(tapply(unlist(z3), z, sum)),
                           spectrum = i)
  })
  # Combine all peak data for all mass/int lists into one data.table
  w <- do.call(rbind, w)
  
  # shift lowest min mass so the matrix is much smaller
  xshift <- z1 - 5
  w$index <- w$index - xshift
  vecLength <- vecLength - xshift
  
  # Create sparse matrix to hold the peak probability data
  # Columns are samples, rows are m/z/intensity probabilities 
  # transform back to actual m/z can be accessed via rownames()
  Matrix::sparseMatrix(i = w$index, 
                       j = w$spectrum,
                       x = w$intensity,
                       dim = c(vecLength, 
                               length(massList)), 
                       dimnames = list(((1:vecLength + xshift) / scaler),
                                       names(massList)))
  
}


























#' High-dimensional representation of mass peaks
#'
#' @param massStart beginning of mass range (as of now, must be smaller than smallest mass)
#' @param massEnd end of mass range (as of now, must be smaller than smallest mass)
#' @param massList list of mass vectors (eg list(1:10, 1:10))
#'
#' @return matrix where rows are samples and columns are variables (m/z preojections)
#' 
#' @importFrom stats dnorm
#' @export
#' @examples
#' \dontrun{
#' masses <- list(Sample_A = c(5000,6000,7000), 
#' Sample_B = c(5000,6010,7005), Sample_C = c(5000,6010,7005))  
#' intensities <- list(Sample_A = rep(1, 3), 
#' Sample_B = rep(1, 3),
#'  Sample_C = rep(1, 3))
#' zx <- binnR(massStart = 3000,
#'             massEnd = 15000,
#'             ppm = 500,
#'             massList = masses,
#'             intensityList = intensities)
#'             }
createFuzzyVectorUnit <- function(massStart,
                                  massEnd,
                                  chunksize,
                                  massList){
  
  breaks <- seq(from = massStart, 
                to = massEnd,
                by = chunksize)
  
  breaks2 <- seq(from = massStart - chunksize / 2, 
                 to = massEnd - chunksize / 2,
                 by = chunksize)
  
  
  abba <- lapply(massList,
                 function(x){
                   
                   a <- unique(c( .bincode(x, breaks, F),
                                  .bincode(x, breaks2, F)))
                   a <- a[!is.na(a)]
                 })
  
  
  
  
  if (!all(lengths(massList) > 0)) {
    warning(paste0(sum(!lengths(massList) > 0), "Empty massList found in createFuzzyVector()"))
  }
  
  # Create sparse matrix to hold the peak probability data
  # Columns are samples, rows are m/z/intensity probabilities 
  # transform back to actual m/z can be accessed via rownames()
  Matrix::sparseMatrix(i = unlist(abba), 
                       j = rep(seq_along(abba), lengths(abba)),
                       x = 1L,
                       dims=c(length(breaks), 
                              length(massList)),
                       dimnames = list(breaks,
                                       names(massList)))
  
}
