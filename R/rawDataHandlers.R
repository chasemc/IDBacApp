#' When user is starting with mzML/mzXML file(s)
#'
#' @param chosenDir user-chosen directory containing mzML and/or mzXML files
#'
#' @return named character vector: values are mzXX file paths, names are the file names
#' @export
#'
#' @examples
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
#' @param excel excel file used for re-naming samples
#' @param tempDir directory to temp mzML files are written to
#' @param chosenDir user-chosen directory containing bruker raw data files
#'
#' @return
#'
#' @examples
startingFromBrukerFlex <- function(chosenDir, 
                                   msconvertPath = "",
                                   excel,
                                   tempDir){
  
  

    tempNames <- base::tempfile(pattern = rep("", length(col)), 
                          tmpdir = tempDir,
                          fileext = ".mzMl")
    
    mzfilePaths <- base::lapply(col, shQuote)
    
    tempNames <- base::normalizePath(tempNames, winslash = "\\", mustWork = FALSE)
    
    tempdirname <- base::dirname(tempNames)[[1]]
    tempdirname <- base::shQuote(tempdirname)
    
    msconvertLocation <- IDBacApp::findMSconvert(msconvertPath)
    msconvertLocation <- base::normalizePath(msconvertLocation,
                                             winslash = "\\",
                                             mustWork = FALSE)
    
    msconvertLocation <- base::shQuote(msconvertLocation)
    
    
    #Command-line MSConvert, converts from proprietary vendor data to mzML
    msconvertCmdLineCommands <- base::lapply(base::seq_along(mzfilePaths), 
                                       function(x){
                                         
                                         (base::paste0(msconvertLocation,
                                                 " ",
                                                 base::paste0(mzfilePaths[[x]],
                                                              collapse = "",
                                                              sep = " "),
                                                 " --mzML --merge -z  --32 -v",
                                                 " --outdir ", tempdirname,
                                                 " --outfile ", tempNames[[x]]))
                                         
                                       })
    
    
    functionTOrunMSCONVERTonCMDline <- function(x){
      system(command = x, 
             invisible = FALSE,
             wait = TRUE)
    }
    
    
    lengthProgress <- length(msconvertCmdLineCommands)
    
    # withProgress(message = 'Conversion in progress',
    #              detail = 'This may take a while...', value = 0, {
    #                for(i in 1:lengthProgress){
    #                  incProgress(1/lengthProgress)
    #                  functionTOrunMSCONVERTonCMDline(msconvertCmdLineCommands[i])
    #                  }
    #              })
    
    
    numCores <- parallel::detectCores()
    numCores <- ifelse(numCores > 2,
                       numCores - 1,
                       1)    
    cl <- parallel::makeCluster(numCores)
    parallel::parLapply(cl,
                        msconvertCmdLineCommands,
                        functionTOrunMSCONVERTonCMDline)
    parallel::stopCluster(cl)
    
    paths <- normalizePath(tempNames)
    
    names(paths) <- names(col)
    
    
  return(paths)
  
  

  
  
}














#' 
#'  Using an excel spreadsheet, get the filepath for msconvert and the user-supplied name
#'
#' @param brukerDataPath path to directory containg bruker files
#' @param excel path to excel file
#'
#' @return named list, names are sample IDs, values are paths
#' @export
#'
#' @examples
brukerDataSpotsandPaths <- function(brukerDataPath){
  
  files <- list.files(brukerDataPath, pattern="acqus", recursive = TRUE, full.names = TRUE)
  
  instrument_MetaFile  <- lapply(files, function(x)  read.delim(x, sep="\n"))
  
  
  # Find Acqu file
  spots <- try(lapply(instrument_MetaFile , function(x) as.character(x[grep("SPOTNO=", x[,1]),])),
               silent = TRUE)
  
  validate(need(length(spots) > 0, "Something happened when trying to get the spot position from the acqus file."))
  names(spots) <- dirname(files)
  
  #Parse the Acqu file for the mass error row
  spots <- sapply(spots, function(x) strsplit(x, "##$SPOTNO= ", fixed = TRUE)[[1]][[2]])
  spots <- base::gsub(">", "" ,spots)
  spots <- base::gsub("<", "" ,spots)
  spots <- base::trimws(spots)
  return(spots)
}
  
  
# 
#   excel <- readxl::read_excel(excel, col_names = FALSE, range ="B2:Y17")
#   userExcel <- as.matrix(userExcel)
#   lets <- LETTERS[1:16]
#   nums <- 1:24
# 
#   aa <- sapply(nums, function(x) paste0(lets, x))
#   aa <- matrix(aa, nrow = 16, ncol = 24)
# 
#   aa <- sapply(spots, function(x) userExcel[which(aa %in% x)])
# 
# 
#   split(names(aa), aa)
# 
# 
# 
# 
#   base::as.matrix(s1)




#' @title Find msconvert.exe
#' @export
#' @rdname findMSconvert NA
#'
#' @param proteoWizardLocation optional filepath of msconvert.exe to be checked for validity
#'
#' @return filepath of the folder containing msconvert.exe
#' @export

findMSconvert <- function(proteoWizardLocation = ""){
  
  # Msconvert only works on Windows so abort function if not on Windows
  os <- IDBacApp::getOS()
  if (os == "windows") {
    
    # Look for msconvert if a path wasn't provided
    if(!file.exists(file.path(dirname(proteoWizardLocation), "msconvert.exe"))) {
      
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