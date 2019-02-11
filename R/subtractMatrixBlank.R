#' Subtract "matrix" sample masses from sample peak lists
#' 
#' @param sampleIds sample IDs corresponding to the peak list
#' @param peakList MALDIquant peak objects
#' @param binTolerance MALDIquant binPeaks tolerance value (ppm / 10^5)

#' @return the peak list modifed by binning then subtractng the matrix sample, or just the binned peak list if no matrix wsa provided

subtractMatrixBlank <- function(sampleIds, 
                                peakList,
                                binTolerance){


binned <- binPeaks(peakList, 
                   method = "strict", 
                   tolerance = binTolerance)

#Next, find which ID contains "matrix", in any capitalization
matrixIndex <- grep("^matrix",
                    sampleIds,
                    ignore.case=TRUE)

if(length(matrixIndex) > 0){
  #peaksa = all samples but remove the matrix sample from the list
  peaksa <- binned[-matrixIndex]
  #peaksb = matrix blank sample
  peaksb <- binned[[matrixIndex]]
  
  for (i in 1:length(peaksa)){
    commonIons <- which(!is.element(peaksa[[i]]@mass, peaksb@mass))
    if(length(commonIons) != 0){   # Without this if statement, peaksa values will be set to 0 if no matrix matches are found == BAD
      peaksa[[i]]@mass <- peaksa[[i]]@mass[-commonIons]
      peaksa[[i]]@intensity <- peaksa[[i]]@intensity[-commonIons]
      peaksa[[i]]@snr <- peaksa[[i]]@snr[-commonIons]
    }
    
  }
  return(peaksa)
}else{
  return(binned)
}

}