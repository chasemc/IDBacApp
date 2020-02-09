#' SQL to MALDIquant
#'
#' @param input matrix/list of martrices , col 1 = masses, col 2 = intensities
#'
#' @return NA
#' 
#'
spectrumMatrixToMALDIqaunt <- function(input){
  
  
  if (class(input) == "list") {
    
    input <- lapply(input, 
                    function(x){
                      MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                     intensity = x[ , 2])
                    })
    
    
  } else if (class(input) == "matrix") {
    
    input <- MALDIquant::createMassSpectrum(mass = input[ , 1],
                                            intensity = input[ , 2])
    input <- list(input)
    
  }
  
  
  
  # Make sure to return input as a MassSpectrumList
  if(MALDIquant::isMassSpectrumList(input)){
    return(input) 
  } else{
    stop("Error in spectrumMatrixToMALDIqaunt: Not a MALDIquant spec list")
    
  }
}

