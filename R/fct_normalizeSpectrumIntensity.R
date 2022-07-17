#' Smooth, remove baseline, and normalize intensity of a spectrum
#'
#' @param spectrum MALDIquant spectrum
#' @param smoothIntensityMethod see ?MALDIquant::smoothIntensity
#' @param removeBaselineMethod see ?MALDIquant::smoothIntensity
#' @param calibrateIntensityMethod see ?MALDIquant::calibrateIntensityMethod
#' @param transformIntensityMethod see ?MALDIquant::transformIntensity
#' @inheritParams MALDIquant::smoothIntensity
#'
#' @return MALDIquant mass spectrum
#'
normalizeSpectrumIntensity  <- function(spectrum,
                                        smoothIntensityMethod = "SavitzkyGolay",
                                        removeBaselineMethod = "TopHat",
                                        calibrateIntensityMethod = "median",
                                        transformIntensityMethod = "sqrt",
                                        halfWindowSize = 20L){
  
  spectrum <- MALDIquant::smoothIntensity(spectrum,
                                          method = smoothIntensityMethod,
                                          halfWindowSize = halfWindowSize) 
  spectrum <- MALDIquant::removeBaseline(spectrum,
                                         method = removeBaselineMethod)
  spectrum <- MALDIquant::calibrateIntensity(spectrum, 
                                             method = calibrateIntensityMethod)   
  
  spectrum@intensity <- (spectrum@intensity / max(spectrum@intensity)) * 100
  
  return(spectrum)
  
}
