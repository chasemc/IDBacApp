

#' binnR
#'
#' @param vectorList  NA
#' @param ppm  NA
#' @param massStart  NA
#' @param massEnd  NA
#'
#' @return NA
#' @export
#'
#' @examples NA
binnR <- function(vectorList,
                  ppm, 
                  massStart,
                  massEnd){
  
  # vectorList: A list of m/z vectors
  # ppm: ppm tolerance 
  # massStart: the first m/z in the IRanges object
  # massEnd: the last m/z in the IRanges object
  
  scaleFactor <- ppm / 10e5 * massStart 
  mrange <- IRanges::IRanges(start = seq(massStart * scaleFactor,
                                         massEnd * scaleFactor, 1),
                             end = seq((massStart * scaleFactor) + 1,
                                       (massEnd * scaleFactor) + 1, 1))
  
  
  mrange <- IRanges::IRanges(seq(massStart, massEnd, scaleFactor),
                             rep(NA, length(seq(massStart, massEnd, scaleFactor))), scaleFactor)
  
  
  
  ranges <- lapply(vectorList, 
                   function(massVector){
                     # get ppm across mass vector
                     pp <- ppm / 10e5 * massVector
                     massVector <- massVector
                     ir1 <- IRanges::IRanges(start = massVector - pp,
                                             end = massVector + pp)
                   }
  )
  lapply(ranges, function(x){
    IRanges::findOverlaps(x, mrange)
    #S4Vectors::unique(S4Vectors::subjectHits(z1))
    
  })
}








#' peakBinner
#'
#' @param peakList should be MALDIquant 
#' @param ppm ppm
#' @param massStart massStart 
#' @param massEnd massEnd 
#'
#' @return list of bin vectors
#' @export
#'
peakBinner <- function(peakList,
                       ppm,
                       massStart,
                       massEnd){

  
  binvec <- IDBacApp::mQuantToMassVec(peakList)
 
   if (is.null(massStart)) {
    massStart <- min(as.vector(unlist(binvec)))
  }
  if (is.null(massEnd)) {
    massStart <- min(as.vector(unlist(binvec)))
  }
  
  binvec <- IDBacApp::binnR(vectorList = binvec,
                            ppm = ppm, 
                            massStart = massStart,
                            massEnd = massEnd)
  
  #  collected <- lapply(zq, function(x) S4Vectors::unique(S4Vectors::subjectHits(x)))
  collected <- lapply(binvec, function(x) S4Vectors::unique(S4Vectors::subjectHits(x)))
  
  cvec <- sort(unique(unlist(collected)))
  
  return(lapply(collected, function(x) match(cvec, x)))
  
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
    
    warning("mQuantToMassVec: not a MALDIquant mass peaks list, trying to convert to MALDIquant mass peaks list")
    peakList <- list(unlist(peakList))
    if(MALDIquant::isMassPeaksList(peakList)){
      
      return(lapply(peakList, function(x) x@mass))
      
    } else {
      warning("mQuantToMassVec: not a MALDIquant mass peaks list, unable to convert to MALDIquant mass peaks list")
    }
    
  }
}






