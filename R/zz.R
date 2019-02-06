
#' Given mzML files, find a protein or small mol spectrum
#'
#' @param proteinORsmall  protein or small
#' @param mzFilePaths paths to mzML files
#'
#' @return
#' @export
#'
#' @examples
spFinder <- function(proteinORsmall,
                     mzFilePaths) {
  
  
  stopTheWhile <- FALSE
  tried <- length(mzFilePaths)
  
  while (!stopTheWhile) {

    randMZ <-  sample(mzFilePaths, 1)
    randMZ <- mzR::openMSfile(randMZ)
    count <- nrow(mzR::header(randMZ))
    
    if (count == 1) {
      maxy <- max(mzR::peaks(randMZ)[,1])
    } else if (count > 1) {
      maxy <- sapply(mzR::peaks(randMZ), function(x) max(x[,1]))
    }    
    
    
    if (proteinORsmall == "protein") {
      if (any(maxy > 5000)) {
        spectrum <- MALDIquantForeign::importMzXml(randMZ@fileName)[sample(which(maxy > 5000), 1)]
      } else {
      }
    } else if (proteinORsmall == "small") {
      if (any(maxy < 5000)) {
        spectrum <- MALDIquantForeign::importMzXml(randMZ@fileName)[sample(which(maxy < 5000), 1)]
      } else {
      }
      
    }
    
    if (!is.null(spectrum)) {
      stopTheWhile <- TRUE
    }
    
    tried <- tried - 1
    if (tried == 0) {
      stopTheWhile <- TRUE
    }
    
  }
  
  return(spectrum)
  
}
