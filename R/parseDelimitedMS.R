
#' parseDelimitedMS
#'
#' @param proteinDirectory NA
#' @param smallMolDirectory NA
#' @param exportDirectory NA
#' @param sampleNames sample names if provided (not used yet)
#'
#' @return NA
#' @export
#'
#' @examples NA
parseDelimitedMS <- function(sampleNames = NULL,
                             proteinDirectory = NULL,
                             smallMolDirectory = NULL,
                             exportDirectory,
                             centroid){
  
  
req((is.null(proteinDirectory) + is.null(smallMolDirectory)) > 0)
  
  dd<<-centroid
  if (is.null(smallMolDirectory)) {
    smallMolFiles <- NULL
  } else {
    smallMolFiles <- list.files(smallMolDirectory, full.names = TRUE)
    sampleNameSM <- tools::file_path_sans_ext(basename(smallMolFiles))
    sampleNameSM <- unlist(lapply(sampleNameSM, function(x) strsplit(x, "-")[[1]][[1]]))
    smallMolFiles <- base::split(smallMolFiles, sampleNameSM)
    smallMolFiles <- lapply(smallMolFiles, function(x) MALDIquantForeign::import(x, centroided = as.logical(centroid)))
    
  }
  
  if (is.null(proteinDirectory)) {
    proteinFiles <- NULL
  } else {
    proteinFiles <- list.files(proteinDirectory, full.names = TRUE)
    sampleNameP <- tools::file_path_sans_ext(basename(proteinFiles))
    sampleNameP <- unlist(lapply(sampleNameP, function(x) strsplit(x, "-")[[1]][[1]]))
    proteinFiles <- base::split(proteinFiles, sampleNameP)
    proteinFiles <- lapply(proteinFiles, function(x) MALDIquantForeign::import(x, centroided = as.logical(centroid)))
    
  }
  
  
  
  keys <- unique(labels(c(proteinFiles, smallMolFiles)))
  
  
  
  lengthProgress <- length(keys)
  count <- 0
  
  withProgress(message = 'Conversion in progress',
               detail = 'This may take a while...', value = 0, {
                 
                 
                 for (i in keys) {
                   incProgress(1/lengthProgress)
                   
                   toMerge <- unlist(c(proteinFiles[i],
                                       smallMolFiles[i]))
                   
                   mzmlPath <- normalizePath(file.path(exportDirectory,
                                                       paste0(i,
                                                              ".mzML")),
                                             mustWork = FALSE)
                   
                   MALDIquantForeign::exportMzMl(as.list(toMerge),
                                                 mzmlPath,
                                                 force = TRUE)
                   
                   
                 }
                 
                 
               })
  
  return(keys)
  
}
