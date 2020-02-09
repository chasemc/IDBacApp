
#' Title
#'
#' @param path directory containing Bruker data, shouldn't contain other things
#'     as this searches recursively
#' @param pool IDBac database pool... e.g. output of idbac_connect()
#' @param ignoreMissing TRUE/FALSE whether to ignore target spots missing ids
#' @param sampleMap base::matrix() representing MALDI target plate with sample ids
#'
#' @return NA
#' 
#'
process_bruker <- function(path,
                           pool,
                           sampleMap,
                           ignoreMissing){
  
  
  # Find spectra and get acquisiton info ------------------------------------
  
  acquisitionInfo <- readBrukerAcqus(path)
  
  # -------------------------------------------------------------------------
  
  fid_paths <- extractBrukerAcquistionInfo(acquisitonInformation = acquisitionInfo,
                                           name =  "file",
                                           type = "")
  
  if (!all(file.exists(fid_paths))) {
    stop("readBrukerAcqus(path) gave file paths for file(s) that R can't find")
  }
  
  
  # Inform user the number of spectra processing ----------------------------
  temp <- extractBrukerAcquistionInfo(acquisitonInformation = acquisitionInfo,
                                      name =  "sampleName",
                                      type = "")
  temp <- table(temp)
  message(paste0("Processing ",
                 temp,
                 " samples from ",
                 names(temp),
                 collapse = "\nand\n"))  
  remove(temp)
  
  # -------------------------------------------------------------------------
  
  
  acquiredSpots <- extractBrukerAcquistionInfo(acquisitonInformation = acquisitionInfo,
                                               name =  "spot",
                                               type = "")
  
  
  
  anyMissing <- findMissingSampleMapIds(spots = acquiredSpots,
                                        sampleMap = sampleMap,
                                        ignoreMissing = ignoreMissing)
  
  
  if (isFALSE(ignoreMissing) && length(anyMissing$missing) > 0L) {
    
    warning("Stopping process_bruker()",
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
    
    
    # splitAcquisition <- split(acquisitionInfo, anyMissing$matching)
    
    
    
    fid_path <- lapply(splitAcquisition, function(x){
      extractBrukerAcquistionInfo(acquisitonInformation = x,
                                  name =  "file",
                                  type = "")
    })
    
    
    
    # Convert with proteowizard -----------------------------------------------
    
    
    
    tempMZDir <- tempdir()
    
    forProcessing <- proteoWizConvert(msconvertPath = "",
                                      samplePathList = fid_path,
                                      convertWhere = tempMZDir)
    
    
    
    sql_fill_version_table(pool = pool)
    sql_fill_locale_table(pool = pool)
    
    
    for (i in base::seq_along(forProcessing$mzFile)) {
      spectraProcessingFunction(rawDataFilePath = forProcessing$mzFile[[i]],
                                sampleID = forProcessing$sampleID[[i]],
                                pool = pool,
                                acquisitionInfo = splitAcquisition[[i]]) # pool connection
    }
    
    
  }
  
}