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

  validate(
    need(length(matrixIndex) > 0, 
         "Matrix blank not found.  Try selecting \"No\" under \"Do you have a matrix blank\" to the left." )
  )
if(!length(matrixIndex) > 0){
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