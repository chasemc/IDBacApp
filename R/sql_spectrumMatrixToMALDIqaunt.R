#' SQL to MALDIquant
#'
#' @param input matrix/list of martrices , col 1 = masses, col 2 = intensities
#'
#' @return NA
#' 
#'
spectrumMatrixToMALDIqaunt <- function(input){
  
  
  if (!inherits(input, "matrix")){
    
    input <- lapply(input, 
                    function(x){
                      x <- MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                     intensity = x[ , 2])
                      
                      x@intensity <- x@intensity / max(x@intensity) * 100
                      x
                      
                    })
    
    
  } else {
    
    input <- MALDIquant::createMassSpectrum(mass = input[ , 1],
                                            intensity = input[ , 2])
    
    input@intensity <- input@intensity / max(input@intensity) * 100
    
    input <- list(input)
    
  }
  
  
  
  # Make sure to return input as a MassSpectrumList
  if (MALDIquant::isMassSpectrumList(input)) {
    return(input) 
  } else{
    stop("Error in spectrumMatrixToMALDIqaunt: Not a MALDIquant spec list")
    
  }
}

