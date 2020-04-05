# Why square root transformation and not log:
#  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
# Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
#"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
#because for all three intensity groups in Table 1, the variance is approximately constant."


#' processSmallMolSpectra
#'
#' @param input MALDIquant mass spectrum list
#' @inheritParams MALDIquant::smoothIntensity
#' @return MALDIquant mass spectrum list
#' 
#'
processSmallMolSpectra <- function(input,
                                   halfWindowSize = 20){
  # Expected warning messages when negative values are replaced with zeros
  # Suppress warnings info:
  # https://github.com/sgibb/MALDIquant/blob/f8258c5b5001453b1816054eb6a9d35c1926dd47/NEWS#L345-L348
  suppressWarnings({
    peaks <- MALDIquant::smoothIntensity(input,
                                         method = "SavitzkyGolay",
                                         halfWindowSize = halfWindowSize) 
  })
  peaks <- MALDIquant::removeBaseline(peaks,
                                      method = "TopHat")
  
  MALDIquant::detectPeaks(peaks, 
                          method = "SuperSmoother",
                          halfWindowSize = 20, 
                          SNR = 1)
}






#' processProteinSpectra
#'
#' @param input MALDIquant mass spectrum list
#' @inheritParams MALDIquant::smoothIntensity
#' @return MALDIquant mass spectrum list
#' 
#'
processProteinSpectra <- function(input,
                                  halfWindowSize = 20){
  
  # Expected warning messages when negative values are replaced with zeros
  # Suppress warnings info:
  # https://github.com/sgibb/MALDIquant/blob/f8258c5b5001453b1816054eb6a9d35c1926dd47/NEWS#L345-L348
  suppressWarnings({
    peaks <- MALDIquant::transformIntensity(input, 
                                            method = "sqrt") 
  })  
  suppressWarnings({
    peaks <- MALDIquant::smoothIntensity(peaks,
                                         method = "SavitzkyGolay", 
                                         halfWindowSize = halfWindowSize) 
  })
  peaks <- MALDIquant::removeBaseline(peaks,
                                      method = "TopHat") 
  
  peaks <- MALDIquant::detectPeaks(peaks, 
                                   method = "MAD", 
                                   halfWindowSize = halfWindowSize, 
                                   SNR = 3)
}

