
#' findBrukerTargetSpots
#'
#' @param brukerDataPath brukerDataPath 
#'
#' @return list of acqus info
#' 
#'
readBrukerAcqus <- function(brukerDataPath){
  
  files <- list.files(brukerDataPath,
                      pattern = "fid", 
                      recursive = TRUE,
                      full.names = TRUE)
  
  lapply(files, function(x){
    readAcqusFile(x)
  })
}