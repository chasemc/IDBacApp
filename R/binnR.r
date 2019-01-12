

#' binnR
#'
#' @param vectorList  NA
#' @param ppm  NA
#' @param refSeqStart  NA
#' @param refSeqEnd  NA
#'
#' @return NA
#' @export
#'
#' @examples NA
binnR <- function(vectorList,
                  ppm, 
                  refSeqStart,
                  refSeqEnd){
  
  # vectorList: A list of m/z vectors
  # ppm: ppm tolerance 
  # refSeqStart: the first m/z in the IRanges object
  # refSeqEnd: the last m/z in the IRanges object
  
  scaleFactor <- ppm / 10e5 * refSeqStart 
  mrange <- IRanges::IRanges(start = seq(refSeqStart * scaleFactor,
                                         refSeqEnd * scaleFactor, 1),
                             end = seq((refSeqStart * scaleFactor) + 1,
                                       (refSeqEnd * scaleFactor) + 1, 1))
  
  
  mrange <- IRanges::IRanges(seq(refSeqStart, refSeqEnd, scaleFactor),
                             rep(NA, length(seq(refSeqStart, refSeqEnd, scaleFactor))), scaleFactor)
  
  
  
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
