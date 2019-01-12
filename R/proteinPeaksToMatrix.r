
#' proteinPeaksToMatrix
#'
#' @param bool NA
#' @param proteinPeaks NA
#'
#' @return NA
#' @export
#'
#' @examples NA
proteinPeaksToMatrix <- function(bool, proteinPeaks){


temp <- NULL
for (i in 1:length(proteinPeaks)) {
  temp <- c(temp, proteinPeaks[[i]]@metaData$Strain)
}
proteinSamples <- factor(temp)
proteinMatrixInnard <- intensityMatrix(proteinPeaks)
rownames(proteinMatrixInnard) <- paste(proteinSamples)
proteinMatrixInnard[is.na(proteinMatrixInnard)] <- 0

if(bool == "0"){
  proteinMatrixInnard
}else if (bool == "1") {
  ifelse(proteinMatrixInnard > 0, 1, 0)

}else{
  proteinMatrixInnard
}

}
