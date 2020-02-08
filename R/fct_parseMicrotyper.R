
#' run_microtyperTomzML setup and loop
#'
#' @param proteinPaths proteinPaths 
#' @param exportDirectory path where temporary mzml are stored
#' @param smallMolPaths smallMolPaths 
#'
#' @return NA
#' @export
#'
run_microtyperTomzML <- function(proteinPaths = NULL,
                                 smallMolPaths = NULL,
                                 exportDirectory = tempdir()){

  if (is.null(proteinPaths)) {
    message("run_microtyperTomzML(): No microtyper protein data provided.")
    # so basename() won't freak
    proteinPaths <- character(0)
  }
  
  
  if (is.null(smallMolPaths)) {
    message("run_microtyperTomzML(): No microtyper small molecule data provided.")
    # so basename() won't freak
    smallMolPaths <- character(0)
  }
  shiny::validate(shiny::need(length(c(proteinPaths, smallMolPaths)) != 0L, "run_microtyperTomzML(): No microtyper data found."))

  key <- base::split(c(proteinPaths, 
                       smallMolPaths),
                     tools::file_path_sans_ext(c(base::basename(proteinPaths), 
                                                 base::basename(smallMolPaths))))
  
  
  
  mzFilePaths <- base::tempfile(pattern = rep("", length(key)), 
                              tmpdir = exportDirectory,
                              fileext = ".mzML")
  
  mzFilePaths <- base::normalizePath(mzFilePaths, winslash = "\\", mustWork = FALSE)
  
  lengthProgress <- length(key)
  
  
  
  shiny::withProgress(message = 'Conversion in progress',
                      detail = 'This may take a while...', value = 0, {
                        
                        for (i in seq_along(key)) {
                          incProgress(1/lengthProgress)
                          microtyperTomzML(key = key[[i]],
                                                     mzFilePaths = mzFilePaths[[i]]) 
                        }
                      })
  
  validate(need(all(file.exists(mzFilePaths)), 
                "Microtyper mzml file not found."))
  
  return(list(mzFilePaths = mzFilePaths,
              sampleIds = names(key)))
}





#' readMicrotyperFiles
#'
#' @param key see microtyperTomzML()  list where filepaths are split() by filename
#' @param mzFilePaths mzFilePaths mzml file paths
#'
#' @return NA
#' @export
#'
microtyperTomzML <- function(key,
                             mzFilePaths){
  
  
  
  specs <- lapply(key,
                  function(x) {     
                    
                    z <- data.table::fread(x, 
                                           skip = 8)
                    MALDIquant::createMassSpectrum(mass = z[[1]], 
                                                   intensity = z[[2]],
                                                   metadata = list(file = x))
                    
                  })
  
  MALDIquantForeign::exportMzMl(x = specs, 
                                path = mzFilePaths,
                                force = TRUE)
}













#' getMicrotyperFiles
#'
#' @param dataDirectory User-chosen directory that contains microtyper-exported files directly inside (doesn't search recursively)
#'
#' @return List of files
#' @export
#'
getMicrotyperFiles <- function(dataDirectory){
  
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

