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
#' @param sampleMap excel file used for re-naming samples
#' @param tempDir directory to temp mzML files are written to
#' @param chosenDir user-chosen directory containing bruker raw data files
#'
#' @return NA
#'
startingFromBrukerFlex <- function(chosenDir, 
                                   msconvertPath = "",
                                   sampleMap,
                                   tempDir){
 
  convertFrom <- base::split(labels(sampleMap),as.character(sampleMap))
  
  convertTo <- base::tempfile(pattern = rep("", length(convertFrom)), 
                              tmpdir = tempDir,
                              fileext = ".mzMl")
  convertTo <- base::normalizePath(convertTo, winslash = "\\", mustWork = FALSE)
  
  convertWhere <- base::dirname(convertTo)[[1]]
  convertWhere <- base::normalizePath(convertWhere, winslash = "\\", mustWork = FALSE)
  convertWhere <- base::shQuote(convertWhere)
  
  msconvertLocation <- IDBacApp::findMSconvert(msconvertPath)
  msconvertLocation <- base::normalizePath(msconvertLocation,
                                           winslash = "\\",
                                           mustWork = FALSE)
  msconvertLocation <- base::shQuote(msconvertLocation)
  
  
  
  
  #Command-line MSConvert, converts from proprietary vendor data to mzML
  msconvertCmdLineCommands <- base::lapply(base::seq_along(convertFrom), 
                                           function(x){
                                             (base::paste0(msconvertLocation,
                                                           " ",
                                                           base::paste0(shQuote(convertFrom[[x]]),
                                                                        collapse = "",
                                                                        sep = " "),
                                                           " --mzML --merge -z  --32 -v",
                                                           " --outdir ", convertWhere,
                                                           
                                                           " --outfile ", convertTo[[x]]))
                                             
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
              sampleID = names(convertFrom)))
  
  
  
  
}




#' 
#'  Using an excel spreadsheet, get the filepath for msconvert and the user-supplied name
#'
#'   from: https://qa.nmrwiki.org/question/143/what-is-the-purpose-of-acqu-and-acqus-files-in-bruker-setup
#'    The s at the end of a parameter file name specifies this file as a status parameterfile. Status parameters are written at the end of an acquisition or also when a FID in a multidimensional experiment is written.
#'    The files without the s are the current parameters. If you change a parameter it will be changed in the files without the s.
#'    Let's assume a dataset where an acquisition has already been done, acqus and acqu contain the same information. You now decide to restart the acquisition but with more scans. You enter a new number for NS. acqu will show this new value and acqus will still show the number that was used to collect the FID that is on disk. Once the new acquisition is finished acqus now also contains the new value. In multidimensional acquisition the value of TD will be updated when a new FID is written to disk. The contents of acqus are printed in parameter listings.
#'    The acqu files with numbers contain the parameters for the indirect dimensions. "acqu2" and acqu2s are for the F1 dimension in a 2D. A 3D will have acqu, acqu2 and acqu3, in a 4D you will also find acqu4 etc.
#' 
#' @param brukerDataPath path to directory containg bruker files
#'
#' @return named list, names are sample IDs, values are paths
#' @export
#'
brukerDataSpotsandPaths <- function(brukerDataPath){
  
  files <- list.files(brukerDataPath, pattern = "acqus", recursive = TRUE, full.names = TRUE)
  
  instrument_MetaFile  <- lapply(files, function(x)  read.delim(x, sep = "\n"))
  
  # Find Acqu file
  spots <- try(lapply(instrument_MetaFile , function(x) as.character(x[grep("SPOTNO=", x[,1]),])),
               silent = TRUE)
  
  validate(need(length(spots) > 0, "Something happened when trying to get the spot position from the acqus file."))
  names(spots) <- dirname(files)
  
  #Parse the Acqu file for the mass error row
  spots <- sapply(spots, function(x) strsplit(x, "##$SPOTNO= ", fixed = TRUE)[[1]][[2]])
  spots <- base::gsub("[[:punct:]]|", "" ,spots)
  spots <- base::trimws(spots)
  return(spots)
}






#' Title
#'
#' @param proteoWizardLocation proteoWizardLocation 
#'
#' @return user's os
#' @export
#'
findMSconvert <- function(proteoWizardLocation = ""){
  
  # Msconvert only works on Windows so abort function if not on Windows
  os <- IDBacApp::getOS()
  if (os == "windows") {
    
    # Look for msconvert if a path wasn't provided
    if (!file.exists(file.path(dirname(proteoWizardLocation), "msconvert.exe"))) {
      
      warning("Given path to msconvert didn't work, trying to auto-find in Programs")
      
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
    } else {
      
      validate("Unfortunately msconvert is not currently available on this operating system, try again on Windows or start with 
               mzML or mzXML files instead of raw-data.")
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
  
  warning(paste0("msconvert location: ", proteoWizardLocation))
  return(proteoWizardLocation)
  
}