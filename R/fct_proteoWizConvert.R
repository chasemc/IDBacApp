#' Convert raw data with msconvert
#'
#' @param msconvertPath path to MSconvert, if none provided, it will search in Programs folder
#' @param convertWhere directory where temp mzML files are written to
#' @param samplePathList list where names are sample ids, and list elements are raw file paths
#'
#' @return list(mzFile = convertTo, sampleID = names(convertFrom)))
#' @export
proteoWizConvert <- function(msconvertPath = "",
                             samplePathList,
                             convertWhere){
  
  
  convertTo <- base::tempfile(pattern = rep("", length(samplePathList)), 
                              tmpdir = convertWhere,
                              fileext = ".mzML")
  
  convertTo <- base::normalizePath(convertTo, winslash = "\\", mustWork = FALSE)
  
  convertWhere <- base::shQuote(convertWhere)
  
  msconvertLocation <- IDBacApp::findMSconvert(msconvertPath)
  
  if (msconvertLocation == "error") {
    
    shiny::showModal(
      modalDialog("Unable to find msconvert.  If you think this is an error, you 
              can file an issue at https://github.com/chasemc/IDBacApp/issues
              Note: msconvert is only for Windows computers, and you might have to download some 
              additional '.NET' software for it to work. To do that, follow the 'Installation'
              instructions at http://proteowizard.sourceforge.net/download.html#Installation
              ", title = NULL, footer = modalButton("Dismiss"),
                  size = c("m"), easyClose = TRUE, fade = TRUE)
      
    )
    Sys.sleep(5)
  } else {
    
    msconvertLocation <- base::shQuote(msconvertLocation)
    
    
    
    #Command-line MSConvert, converts from proprietary vendor data to mzML
    # Nope to vectorized, loop through because mult files can be attributed to one sample id
    msconvertCmdLineCommands <- base::lapply(base::seq_along(samplePathList), 
                                             function(x){
                                               (base::paste0(msconvertLocation,
                                                             " ",
                                                             base::paste0(shQuote(samplePathList[[x]]),
                                                                          collapse = "",
                                                                          sep = " "),
                                                             " --mzML --merge -z  --32 -v",
                                                             " --outdir ", convertWhere,
                                                             
                                                             " --outfile ", base::shQuote(convertTo[[x]])))
                                               
                                             })
    
    
    functionTOrunMSCONVERTonCMDline <- function(x){
      system(command = x, 
             invisible = FALSE,
             wait = TRUE)
    }
    
    
    lengthProgress <- length(msconvertCmdLineCommands)
    
    
    
    numCores <- parallel::detectCores()
    numCores <- ifelse(numCores > 2,
                       numCores - 1,
                       1)    
    cl <- parallel::makeCluster(numCores)
    parallel::parLapply(cl,
                        msconvertCmdLineCommands,
                        functionTOrunMSCONVERTonCMDline)
    parallel::stopCluster(cl)
    
    
    
    validate(need(all(file.exists(convertTo)), 
                  cbind(convertTo, exists(convertTo))
    ))
    
    
    return(list(mzFile = convertTo,
                sampleID = names(samplePathList)))
    
  }
  
  
}






