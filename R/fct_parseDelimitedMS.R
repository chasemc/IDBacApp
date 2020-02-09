
#' parseDelimitedMS
#'
#' @param proteinPaths proteinPaths 
#' @param proteinNames proteinNames 
#' @param smallMolPaths smallMolPaths 
#' @param smallMolNames smallMolNames names of the 
#' @param centroid Do the files contain profile or centroid data (currently implemented as just
#'     one TRUE/FALSE, but could mappply) 
#' @param exportDirectory NA
#'
#' @return NA
#' 
#'
parseDelimitedMS <- function(proteinPaths,
                             proteinNames,
                             smallMolPaths,
                             smallMolNames,
                             exportDirectory,
                             centroid){
  
  req(length(proteinPaths) == length(proteinNames))
  req(length(smallMolPaths) == length(smallMolNames))
  
  
  popup3()
  
  combPaths <- c(proteinPaths,
                 smallMolPaths)
  
  #lapply in case someone provides different file types at same time -_-
  importedFiles <- unlist(lapply(combPaths, 
                                 function(x){ 
                                   MALDIquantForeign::import(x,
                                                             centroided = as.logical(centroid))
                                 }))
  

  combNames <- c(proteinNames, 
                 smallMolNames)
  
  mzFilePaths <- file.path(exportDirectory,
                           paste0(combNames,
                                  ".mzML"))
  
  mzFilePaths <- normalizePath(mzFilePaths, 
                               mustWork = FALSE,
                               winslash = "/")
  
  key <- base::split(importedFiles, mzFilePaths)
  
  
  lengthProgress <- length(key)
  count <- 0
  
  # withProgress doesn't currently work outside shiny
  if (!is.null(shiny::getDefaultReactiveDomain())) { 
  withProgress(message = 'Conversion in progress',
               detail = 'This may take a while...', value = 0, {
                 
                 
                 for (i in seq_along(key)) {
                   incProgress(1/lengthProgress)
                   
                   
                   MALDIquantForeign::exportMzMl(x = as.list(key[[i]]),
                                                 path = names(key)[[i]],
                                                 force = TRUE)
                   
                 }
                 
                 
               })
    
  } else {
    for (i in seq_along(key)) {

      MALDIquantForeign::exportMzMl(x = as.list(key[[i]]),
                                    path = names(key)[[i]],
                                    force = TRUE)
      
    }
  }
  
  
  return(list(mzFilePaths = mzFilePaths,
              sampleIds = combNames))
  
}
