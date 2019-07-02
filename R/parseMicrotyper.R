
#' run_microtyperTomzML setup and loop
#'
#' @param proteinPaths proteinPaths 
#' @param smallMolPaths smallMolPaths 
#' @param exportDirectory exportDirectory
#'
#' @return NA
#' @export
#'
run_microtyperTomzML <- function(proteinPaths,
                                 smallMolPaths,
                                 exportDirectory){
  
  
  
  
  key <- base::split(c(proteinPaths, 
                       smallMolPaths),
                     tools::file_path_sans_ext(c(base::basename(proteinPaths), 
                                                 base::basename(smallMolPaths))))
  
  
  mzFilePaths <- file.path(exportDirectory,
                           paste0(names(key),
                                  ".mzML"))
  
  mzFilePaths <- normalizePath(mzFilePaths, 
                               mustWork = FALSE)
  
  
  
  lengthProgress <- length(key)
  
  
  
  shiny::withProgress(message = 'Conversion in progress',
                      detail = 'This may take a while...', value = 0, {
                        
                        
                        for (i in seq_along(key)) {
                          incProgress(1/lengthProgress)
                          
                        IDBacApp::readMicrotyperFiles(key = key,
                                    mzFilePaths = mzFilePaths,
                                    iteration = i) 
                          
                        }
                        
                      })
  return(list(mzFilePaths = mzFilePaths,
              sampleIds = names(key)))
}





#' readMicrotyperFiles
#'
#' @param key see microtyperTomzML()  list where filepaths are split() by filename
#' @param mzFilePaths mzFilePaths mzml file paths
#' @param iteration loop iteration
#'
#' @return NA
#' @export
#'
microtyperTomzML <- function(key,
                      mzFilePaths,
                      iteration){
  
  
  
  specs <- lapply(key[[iteration]],
                  function(x) {     
                    
                    z <- utils::read.table(x, 
                                           skip = 8,
                                           sep = ";", 
                                           row.names = NULL,
                                           colClasses = "numeric",
                                           fileEncoding = "UTF-16LE")
                    MALDIquant::createMassSpectrum(mass = z[ , 1], 
                                                   intensity = z[ , 2],
                                                   metaData = list(file = x))
                    
                  })
  
  MALDIquantForeign::exportMzMl(x = specs, 
                                path = mzFilePaths[[iteration]],
                                force = TRUE)
}



  
  








#' getMicrotyperFiles
#'
#' @param dataDirectory User-chosen directory that contains microtyper-exported files directly inside (doesn't search recursively)
#'
#' @return List of files
#' @export
#'
#' @examples
getMicrotyperFiles <- function(dataDirectory = delimitedLocationP()){
  
  files <- NULL
  
  if (is.null(dataDirectory)) {
    warning("getMicrotyperFiles(): Must select a directory.")
  } else if (length(dataDirectory) > 1) {
    warning("getMicrotyperFiles(): Select only one directory.")
    
  } else if (!dir.exists(dataDirectory)) {
    warning("getMicrotyperFiles(): Selected directory not found.")
    
  } else {
    # Get the folders contained directly within the chosen folder.
    files <- list.files(dataDirectory, 
                        recursive = FALSE, 
                        full.names = TRUE,
                        pattern = "(.txt)$") 
    
    
  }
  
  if (length(files) == 0L ){
    warning("getMicrotyperFiles(): Returning NULL")
  }
  
  return(files)
  
  
}

