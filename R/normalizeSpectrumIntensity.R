#' Smooth, remove baseline, and normalize intensity of a spectrum
#'
#' @param spectrum spectrum
#'
#' @return normalized MALDIquant massSpectrum
#' @export
#'
normalizeSpectrumIntensity  <- function(spectrum){
  
  spectrum <- MALDIquant::smoothIntensity(spectrum,
                                          method = "SavitzkyGolay",
                                          halfWindowSize = 20) 
  spectrum <- MALDIquant::removeBaseline(spectrum,
                                         method = "TopHat")
  spectrum <- MALDIquant::calibrateIntensity(spectrum, 
                                             method = "median")   
  
  spectrum@intensity <- (spectrum@intensity / max(spectrum@intensity)) * 100
  
  return(spectrum)
  
  
}

# 
# 
# ## baseline correction
# b <- removeBaseline(fiedler2009subset)
# 
# ## calibrate intensity values
# calibrateIntensity(b, method="TIC")
# 
# ## calibrate intensity values using TIC for a specific mass range
# calibrateIntensity(b, method="TIC", range=c(3000, 5000))