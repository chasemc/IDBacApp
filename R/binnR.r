#' 
#' #' Bin MALDI peaks
#' #'
#' #' @param peakList list of MALDIquant peak objects
#' #' @param ppm ppm
#' #' @param massStart massStart 
#' #' @param massEnd massEnd 
#' #'
#' #' @return list of bin vectors
#' #' @export
#' #'
#' peakBinner <- function(peakList,
#'                        ppm = 300,
#'                        massStart = 3000,
#'                        massEnd = 15000){
#'   # new binning algo not ready
#'   #https://github.com/chasemc/IDBacApp/commit/d676e8329e08a149ca11913db663d636b51e6e68#diff-669488b95a9fc327f0603fbfddd4100c
#'   nams <- names(peakList)
#'   peakList <- MALDIquant::binPeaks(peakList, method = "relaxed", tolerance = .02)
#'   peakList <- MALDIquant::intensityMatrix(peakList)
#'   peakList[is.na(peakList)] <- 0
#'   rownames(peakList) <- nams
#'   peakList
#' }




#' MALDIquant to Mass Vectors
#'
#' @param peakList MALDIquant peak list
#'
#' @return list of mass vectors
#' @export
#'
mQuantToMassVec <- function(peakList){
  
  if (MALDIquant::isMassPeaksList(peakList)) {
    
    return(lapply(peakList, function(x) x@mass))
    
  } else {
    
    peakList <- list(unlist(peakList))
    if (MALDIquant::isMassPeaksList(peakList)) {
      
      return(lapply(peakList, function(x) x@mass))
      
    } else {
      warning("mQuantToMassVec: not a MALDIquant mass peaks list, unable to convert to MALDIquant mass peaks list")
      return(NULL)
    }
    
  }
}






#' Peak List Binner
#'
#' @param massStart beginning of mass range (as of now, must be smaller than smallest mass)
#' @param massEnd end of mass range (as of now, must be smaller than smallest mass)
#' @param ppm ppm tolerance
#' @param massList list of mass vectors (eg list(1:10, 1:10))
#' @param intensityList list of intesity vectors (eg list(1:10, 1:10))
#'
#' @return matrix where rows are samples and columns are variables (m/z preojections)
#' @export
#'
#' @examples
#' \dontrun{
#' masses <- list(Sample_A = c(5000,6000,7000), Sample_B = c(5000,6010,7005), Sample_C = c(5000,6010,7005))  
#' intensities <- list(Sample_A = rep(1, 3), Sample_B = rep(1, 3), Sample_C = rep(1, 3))
#' zx <- binnR(massStart = 3000,
#'             massEnd = 15000,
#'             ppm = 500,
#'             massList = masses,
#'             intensityList = intensities)
#'             }
peakBinner <- function(massStart,
                       massEnd,
                       ppm,
                       massList,
                       intensityList){
  
  shiny::validate(shiny::need(ppm > 300, "Select a ppm > 300"))
  
  scale_ppm <- function(mass,
                        ppm){
    1L / ((ppm / 1000000L) * mass)
  }
  
  
  binner_xShift <- function(massStart,
                            scaler){
    round((massStart*scaler) - 2) 
  }
  scaler <- scale_ppm(ppm = ppm, 
                      mass = massStart)
  
  toSub <- binner_xShift(massStart = massStart,
                         scaler = scaler) 
  
  
  z1 <- as.integer(round((massEnd * scaler)))
  z2 <- as.integer(round((massEnd * ppm) / 1000000L))
  vecLength <- z1 + z2 + 2
  
  #  if (length(massList) * vecLength < 4e6) {
  builtM <- base::matrix(0, nrow = length(massList), ncol = vecLength) 
  #  } else {
  #    builtM <- Matrix::Matrix(0, nrow = length(massList), ncol = vecLength, sparse = TRUE) 
  #  }
  
  # mm returns a list of lists. each list element contains a list of length 3:
  # 1- centroid - ppm 
  # 2- centroid
  # 3- centroid + ppm
  mm <- lapply(massList, function(x){
    # scale and shift to begin at 1
    #z1 <- as.integer(round((x * scaler) - toSub))
    z1 <- as.integer(round((x * scaler)))
    z2 <- as.integer(round((x * ppm) / 1000000L))
    
    list(z1 - z2,
         z1,
         z1 + z2)
    
  })
  
  # Create a distribution of "intensity" across each ppm range of each peak
  # loop across all samples (spectra) 
  for (i in seq_along(mm)) {
    
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
      mm[[i]][[3]])
    
    # Multiple each centroid's intensity against its above calculated distribution
    #z3 <-  mapply(function(x, y) x * y, z2, intensityList[[i]])
    z3 <-  mapply(function(x, y) x * y, z2, 1000)
    
    for (ii  in seq_along(z)){
      
      builtM[i, z[[ii]]] <- builtM[i, z[[ii]]] + (z3[[ii]])
      
    }
  } 
  
  rownames(builtM) <- names(massList)
  
  return(builtM)
  
}
