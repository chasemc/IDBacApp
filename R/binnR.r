
#' Bin MALDI peaks
#'
#' @param peakList list of MALDIquant peak objects
#' @param ppm ppm
#' @param massStart massStart 
#' @param massEnd massEnd 
#'
#' @return list of bin vectors
#' @export
#'
peakBinner <- function(peakList,
                       ppm = 300,
                       massStart = 3000,
                       massEnd = 15000){
# new binning algo not ready
#https://github.com/chasemc/IDBacApp/commit/d676e8329e08a149ca11913db663d636b51e6e68#diff-669488b95a9fc327f0603fbfddd4100c
  nams <- names(peakList)
  peakList <- MALDIquant::binPeaks(peakList, method = "relaxed", tolerance = .02)
  peakList <- MALDIquant::intensityMatrix(peakList)
  peakList[is.na(peakList)] <- 0
  rownames(peakList) <- nams
  peakList
}




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






