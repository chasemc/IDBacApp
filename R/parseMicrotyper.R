
#' Parse Microtyper files
#'
#' @param proteinPaths proteinPaths 
#' @param proteinNames proteinNames 
#' @param smallMolPaths smallMolPaths 
#' @param smallMolNames smallMolNames 
#' @param centroid centroid 
#' @param exportDirectory NA
#'
#' @return NA
#' @export
#'
microtyperTomzML <- function(proteinPaths,
                             proteinNames,
                             smallMolPaths,
                             smallMolNames,
                             exportDirectory,
                             centroid = FALSE){
  

  
  req(length(proteinPaths) == length(proteinNames))
  req(length(smallMolPaths) == length(smallMolNames))
  
  
  IDBacApp::popup3()
  
  
  key <- base::split(c(proteinPaths, smallMolPaths),
                      tools::file_path_sans_ext(c(proteinNames, smallMolNames)))
  
  
  mzFilePaths <- file.path(exportDirectory,
                           paste0(names(key),
                                  ".mzML"))
  
  mzFilePaths <- normalizePath(mzFilePaths, 
                                mustWork = FALSE)
  
  
  
  
  withProgress(message = 'Conversion in progress',
               detail = 'This may take a while...', value = 0, {
                 
                 lengthProgress <- length(key)
                 
                 
                 for (i in seq_along(key)) {
                 incProgress(1/lengthProgress)
                   
                   specs <- lapply(key[[i]],
                                   function(x) {     
                                     
                                     z <- read.table(x, 
                                                     skip = 8,
                                                     sep = ";", 
                                                     row.names = NULL,
                                                     colClasses = "numeric",
                                                     fileEncoding = "UTF-16LE")
                                     return(
                                       MALDIquantForeign:::.createMassObject(mass = z[ , 1], 
                                                                             intensity = z[ , 2],
                                                                             snr = NULL, 
                                                                             metaData = list(file = x),
                                                                             centroided = as.logical(centroid), 
                                                                             massRange = c(0, Inf), 
                                                                             minIntensity = 0, 
                                                                             verbose = FALSE)
                                       
                                       
                                     )
                                   }
                   )
                   
                   
                   
                   
                   
                   MALDIquantForeign::exportMzMl(x = specs, 
                                                 path = mzFilePaths[[i]],
                                                 force = TRUE)
                   
                 }
                 
                 
               })
  
  
  
  
  
  
  
  return(list(mzFilePaths = mzFilePaths,
              sampleIds = names(key)))
  
}
