#' When user is starting with mzML/mzXML file(s)
#'
#' @param chosenDir user-chosen directory containing mzML and/or mzXML files
#'
#' @return named character vector: values are mzXX file paths, names are the file names
#' @export
#'
startingFromMZ <- function(chosenDir){
  
  paths <- list.files(chosenDir,
                      recursive = TRUE,
                      full.names = TRUE,
                      pattern = ".mz") 
  
  validate(need(length(paths) > 0), "Didn't find any mzML or mzXML files here.")
  
  names(paths) <- tools::file_path_sans_ext(base::basename(paths))
  
  return(paths)
  
}





#' When user is starting with Bruker Flex file(s)
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








#' Title
#'
#' @param proteoWizardLocation proteoWizardLocation 
#'
#' @return user's os
#' @export
#'
findMSconvert <- function(proteoWizardLocation = ""){
  
  if(length(proteoWizardLocation) > 1) {
    proteoWizardLocation <- proteoWizardLocation[[1]]
  }
  
  if(!is.character(proteoWizardLocation)) {
    proteoWizardLocation <- ""
  }
  
  
  a <- system.file(package = "IDBacApp")
  a <- file.path(a, "pwiz")
  
  
  if (file.exists(file.path(a, "msconvert.exe"))) {
    proteoWizardLocation <- file.path(a, "msconvert.exe")
  }
  
  if (!file.exists(proteoWizardLocation)) {
    # Msconvert only works on Windows so abort function if not on Windows
    os <- IDBacApp::getOS()
    if (os != "windows") {
      
      message("Unfortunately msconvert is not currently available on this operating system, try again on Windows or start with 
               mzML or mzXML files instead of raw-data.")
      
    } else {
      
      # Look for msconvert if a path wasn't provided/ msconvert.exe isn't at specified path
      if (!file.exists(file.path(dirname(proteoWizardLocation), "msconvert.exe"))) {
        
        message("Given path to msconvert didn't work, trying to auto-find in Programs")
        
        # Check 64
        proteoWizardLocation <- base::shell(cmd = "ECHO %ProgramFiles%\\ProteoWizard", 
                                            translate = TRUE, 
                                            intern = T)
        proteoWizardLocation <- base::list.files(proteoWizardLocation,
                                                 recursive = TRUE, 
                                                 pattern = "msconvert.exe",
                                                 full.names = TRUE)
        
        if(length(proteoWizardLocation) == 0) {
          # Check 32
          proteoWizardLocation <- base::shell(cmd = "ECHO %programfiles(x86)%\\ProteoWizard", 
                                              translate = TRUE, 
                                              intern = T)
          proteoWizardLocation <- base::list.files(proteoWizardLocation,
                                                   recursive = TRUE, 
                                                   pattern = "msconvert.exe",
                                                   full.names = TRUE)
          
        }
      } 
      
      
      foundMSconvert <- base::file.exists(proteoWizardLocation)
      
      if (length(foundMSconvert) > 0) {
        
        proteoWizardLocation <- proteoWizardLocation[[1]]
        proteoWizardLocation <- normalizePath(proteoWizardLocation,
                                              winslash = "\\",
                                              mustWork = NA)
        message(paste0("msconvert location: ", proteoWizardLocation))
      } else {
        proteoWizardLocation <- "error"
        
      } 
    }
  }
  return(proteoWizardLocation)
}