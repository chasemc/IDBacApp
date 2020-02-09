#' Find proteowizard's msconvert.exe
#'
#' @param proteoWizardLocation optional path to directory containing msconvert.exe
#'
#' @return user's os
#' 
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
    os <- getOS()
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
                                              winslash = "/",
                                              mustWork = NA)
        message(paste0("msconvert location: ", proteoWizardLocation))
      } else {
        proteoWizardLocation <- "error"
        
      } 
    }
  }
  return(proteoWizardLocation)
}