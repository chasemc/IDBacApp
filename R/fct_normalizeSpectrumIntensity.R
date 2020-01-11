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
