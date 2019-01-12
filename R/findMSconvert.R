#' @title Find msconvert.exe
#' @export
#' @rdname findMSconvert NA
#'
#' @param proteoWizardLocation optional filepath of msconvert.exe to be checked for validity
#'
#' @return filepath of the folder containing msconvert.exe
#' @export

findMSconvert <- function(proteoWizardLocation = character()){
  
  # Msconvert only works on Windows so abort function if not on Windows
  os <- IDBacApp::getOS()
  if (os == "windows") {
    
    # Look for msconvert if a path wasn't provided
    if(length(proteoWizardLocation) == 0) {
      
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
        proteoWizardLocation2 <- base::shell(cmd = "ECHO %programfiles(x86)%\\ProteoWizard", 
                                             translate = TRUE, 
                                             intern = T)
        
        proteoWizardLocation <- base::list.files(proteoWizardLocation,
                                                 recursive = TRUE, 
                                                 pattern = "msconvert.exe",
                                                 full.names = TRUE)
      }
    }
    
    foundMSconvert <- tryCatch(base::file.exists(proteoWizardLocation),
                               error = function(e) return(FALSE))[[1]]
    
  if(foundMSconvert){
 
    proteoWizardLocation <- base::normalizePath(proteoWizardLocation[[1]])
    
  } else {
    proteoWizardLocation <- "error"
     warning("Unable to find msconvert.exe")
    
  }
}


return(proteoWizardLocation)

}