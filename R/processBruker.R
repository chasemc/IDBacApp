
#' Title
#'
#' @param path directory containing Bruker data, shouldn't contain other things
#'     as this searches recursively
#' @param pool IDBac database pool... e.g. output of idbac_connect()[[1]]
#' @param ignoreMissing TRUE/FALSE whether to ignore target spots missing ids
#' @param sampleMap base::matrix() representing MALDI target plate with sample ids
#'
#' @return NA
#' @export
#'
process_bruker <- function(path,
                           pool,
                           sampleMap,
                           ignoreMissing){
  
  
  
  
  path <- "C:/Users/CMC/Documents/GitHub/programming_idbac/data/bruker_autoflex"
  
  
  
  # Find spectra and get acquisiton info ------------------------------------
  
  acquisitionInfo <- IDBacApp::readBrukerAcqus(path)
  
  # -------------------------------------------------------------------------
  
  fid_paths <- IDBacApp::findBrukerInfo(acquisitonInformation = acquisitionInfo,
                                        name =  "file",
                                        type = "")
  
  if (!all(file.exists(fid_paths))) {
    stop("IDBacApp::readBrukerAcqus(path) gave file paths for file(s) that R can't find")
  }
  
  
  # Inform user the number of spectra processing ----------------------------
  temp <- IDBacApp::findBrukerInfo(acquisitonInformation = acquisitionInfo,
                                   name =  "sampleName",
                                   type = "")
  temp <- table(temp)
  message(paste0("Processing ",
                 temp,
                 " samples in ",
                 names(temp),
                 collapse = "\n"))  
  remove(temp)
  
  # -------------------------------------------------------------------------
  
  
  acquiredSpots <- IDBacApp::findBrukerInfo(acquisitonInformation = acquisitionInfo,
                                            name =  "spot",
                                            type = "")
  
  
  
  anyMissing <- IDBacApp::findMissingSampleMapIds(spots = acquiredSpots,
                                                  sampleMap = sampleMap,
                                                  ignoreMissing = ignoreMissing)
  
  
  if (isFALSE(ignoreMissing) && length(anyMissing$missing) > 0L) {
    
    warning("Stopping IDBacApp::process_bruker()",
            "\nData was found for sample spots ",
            paste0(names(anyMissing$missing), collapse = ", "),
            "\nbut missing in process_bruker(sampleMap = )\n",
            "This can be ignored by setting process_bruker(ignoreMissing = TRUE)")
    
  } else {
    
    
    # Splits into a list of lists 
    #  Top level = sample ids
    #  Next level contains acqu info for each spectra 
    
    splitAcquisition <- split(acquisitionInfo,
                              anyMissing$matching[match(acquiredSpots, names(anyMissing$matching))])
    
    
    splitAcquisition <- split(acquisitionInfo, anyMissing$matching)
  
    

    fid_path <- lapply(splitAcquisition, function(x){
      IDBacApp::findBrukerInfo(acquisitonInformation = x,
                                                     name =  "file",
                                                     type = "")
    })
    
    

# Convert with proteowizard -----------------------------------------------

    
    
    tempMZDir <- tempdir()
    
    forProcessing <- IDBacApp::proteoWizConvert(msconvertPath = "",
                                                samplePathList = files,
                                                convertWhere = tempMZDir)
    forProcessing
    
    
    
    
    
    
    
    
    
    
    
    
    
    userDBpool <- IDBacApp::createNewSQLITEdb(newExperimentName = params$experiment_name,
                                              sqlDirectory = params$output_dir)[[1]]
    userDB <- pool::poolCheckout(userDBpool)
    
    for (i in base::seq_along(forProcessing$mzFile)) {
      IDBacApp::spectraProcessingFunction(rawDataFilePath = forProcessing$mzFile[[i]],
                                          sampleID = forProcessing$sampleID[[i]],
                                          userDBCon = userDB,
                                          acquisitionInfo = splitAcquisition[[i]]) # pool connection
    }
    
    
    pool::poolClose(userDBpool)
    
    
    
    
    
    
  }
  
}