# Why square root transformation and not log:
#  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
# Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
#"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
#because for all three intensity groups in Table 1, the variance is approximately constant."



#' Process small molecule spectra with MALDIquant
#' 
#' @param input MALDIquant mass spectrum list
#' @param smoothIntensityMethod see ?MALDIquant::smoothIntensity
#' @param removeBaselineMethod see ?MALDIquant::smoothIntensity
#' @param detectPeaksMethod see ?MALDIquant::detectPeaks
#' @param minSNR see ?MALDIquant::detectPeaks
#' @inheritParams MALDIquant::smoothIntensity
#'
#' @return MALDIquant mass spectrum list
#'
processSmallMolSpectra <- function(input,
                                   smoothIntensityMethod = "SavitzkyGolay",
                                   removeBaselineMethod = "TopHat",
                                   detectPeaksMethod = "SuperSmoother",
                                   halfWindowSize = 20L,
                                   minSNR = 1){
  # Expected warning messages when negative values are replaced with zeros
  # Suppress warnings info:
  # https://github.com/sgibb/MALDIquant/blob/f8258c5b5001453b1816054eb6a9d35c1926dd47/NEWS#L345-L348
  suppressWarnings({
    input <- MALDIquant::smoothIntensity(object = input,
                                         method = smoothIntensityMethod,
                                         halfWindowSize = halfWindowSize) 
  })
  
  input <- MALDIquant::removeBaseline(object = input,
                                      method = removeBaselineMethod)
  
  MALDIquant::detectPeaks(object = input, 
                          method = detectPeaksMethod,
                          SNR = minSNR,
                          halfWindowSize = halfWindowSize) 
  
  
}






#' Process protein spectra with MALDIquant
#' 
#' @param input MALDIquant mass spectrum list
#' @param smoothIntensityMethod see ?MALDIquant::smoothIntensity
#' @param removeBaselineMethod see ?MALDIquant::smoothIntensity
#' @param detectPeaksMethod see ?MALDIquant::detectPeaks
#' @param minSNR see ?MALDIquant::detectPeaks
#' @param transformIntensityMethod see ?MALDIquant::transformIntensity
#' @inheritParams MALDIquant::smoothIntensity
#'
#' @return MALDIquant mass spectrum list
#' @export
#'
processProteinSpectra <- function(smoothIntensityMethod = "SavitzkyGolay",
                                  removeBaselineMethod = "TopHat",
                                  detectPeaksMethod = "MAD",
                                  transformIntensityMethod = "sqrt",
                                  halfWindowSize = 20L,
                                  minSNR = 3){
  
  # Expected warning messages when negative values are replaced with zeros
  # Suppress warnings info:
  # https://github.com/sgibb/MALDIquant/blob/f8258c5b5001453b1816054eb6a9d35c1926dd47/NEWS#L345-L348
  suppressWarnings({
    input <- MALDIquant::transformIntensity(object = input, 
                                            method = transformIntensityMethod) 
  })  
  suppressWarnings({
    input <- MALDIquant::smoothIntensity(object = input,
                                         method = smoothIntensityMethod, 
                                         halfWindowSize = halfWindowSize) 
  })
  input <- MALDIquant::removeBaseline(object = input,
                                      method = removeBaselineMethod) 
  
  input <- MALDIquant::detectPeaks(object = input, 
                                   method = detectPeaksMethod, 
                                   halfWindowSize = halfWindowSize, 
                                   SNR = 3)
  if(inherits(input, "list")) {
    
    if (inherits(input, "MassPeaks")) {
      input@intensity <- (input@intensity / max(input@intensity)) * 100
    }
    
    input <- lapply(input, function(x){
      
      if(inherits(x, "list")){
        x <- lapply(x, function(x){
          x@intensity <- x@intensity / max(x@intensity) * 100
          x
        })
      } else{
        x@intensity <- x@intensity / max(x@intensity) * 100
      }
      x
    })
  } 
  
  return(input)
  
}

