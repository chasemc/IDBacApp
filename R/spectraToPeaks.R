



# Why square root transformation and not log:
#  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
# Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
#"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
#because for all three intensity groups in Table 1, the variance is approximately constant."









#' processSmallMolSpectra
#'
#' @param input MALDIquant mass spectrum list
#'
#' @return MALDIquant mass spectrum list
#' @export
#'
processSmallMolSpectra <- function(input){
  peaks <- MALDIquant::smoothIntensity(input,
                                       method = "SavitzkyGolay",
                                       halfWindowSize = 20) 
  peaks <- MALDIquant::removeBaseline(peaks,
                                      method = "TopHat")
  peaks <- MALDIquant::calibrateIntensity(peaks, 
                                          method = "TIC") 
  MALDIquant::detectPeaks(peaks, 
                          method = "SuperSmoother",
                          halfWindowSize = 20, 
                          SNR = 1)
}






#' processProteinSpectra
#'
#' @param input MALDIquant mass spectrum list
#'
#' @return MALDIquant mass spectrum list
#' @export
#'
processProteinSpectra <- function(input){
  
  # No way to turn off warnings in MALDIquant:::.replaceNegativeIntensityValues()
  suppressWarnings(
    peaks <- MALDIquant::transformIntensity(input, 
                                            method = "sqrt") 
  )
  peaks <- MALDIquant::smoothIntensity(peaks,
                                       method = "SavitzkyGolay", 
                                       halfWindowSize = 20) 
  peaks <- MALDIquant::removeBaseline(peaks,
                                      method = "TopHat") 
  peaks <- MALDIquant::detectPeaks(peaks, 
                                   method = "MAD", 
                                   halfWindowSize = 20, 
                                   SNR = 3)
  lapply(peaks, 
         function(x){
           x@intensity <- x@intensity * (100 / max(x@intensity))
           x
         })
}

