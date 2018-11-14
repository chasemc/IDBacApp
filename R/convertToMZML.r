
convertToMzml <- function(mzmlRawFileDirectory,
                          mzmlRawFilesLocation,
                          pwizFolderLocation,
                          outDir){

  
  
if(!is.null(mzmlRawFileDirectory)){
  
  # vector of filepaths
  mzfilePath <- normalizePath(mzmlRawFilesLocation, winslash = "/" )
  
  # vector of file names, no extensions
  filenames <- basename(tools::file_path_sans_ext(mzfilePath))

}else{
  # Get names from excel
  fullZ <- spectraConversion
}

  
  
  tempNames <- tempfile(pattern = rep("", length(filenames)))




list(mzfilePath = mzfilePath,
     filenames = filenames,
     tempNames = tempNames)


#Command-line MSConvert, converts from proprietary vendor data to open mzML
msconvertCmdLineCommands <- lapply(fullZ, function(x){
  #Finds the msconvert.exe program which is located the in pwiz folder which is two folders up ("..\\..\\") from the directory in which the IDBac shiny app initiates from
  paste0(shQuote(file.path(pwizFolderLocation,
                           "msconvert.exe")),
         # sets up the command to pass to MSConvert in commandline, with variables for the input files (x$UserInput.y) and for where the newly created mzML files will be saved
         " ",
         paste0(shQuote(normalizePath(x$UserInput.y)), 
                collapse = "",
                sep=" "),
         # "--noindex --mzML --merge -z",
         "--noindex --mzML --merge -z  --32",
         " -o ",
         shQuote(normalizePath(outDir)),
         " --outfile ",
         shQuote(paste0(x$tempFile, ".mzML"))
  )
}
)

functionTOrunMSCONVERTonCMDline<-function(x){
  system(command = as.character(x))
}


lengthProgress <- length(msconvertCmdLineCommands)

# withProgress(message = 'Conversion in progress',
#              detail = 'This may take a while...', value = 0, {
#                for(i in 1:lengthProgress){
#                  incProgress(1/lengthProgress)
#                  functionTOrunMSCONVERTonCMDline(msconvertCmdLineCommands[i])
#                  }
#              })

# TODO: Add parallel msconvert UI

numCores <- parallel::detectCores()
cl <- parallel::makeCluster(numCores)
parallel::parLapply(cl,msconvertCmdLineCommands,functionTOrunMSCONVERTonCMDline)
parallel::stopCluster(cl)

#Single process with sapply instead of parsapply
#sapply(fileList,function(x)spectraProcessingFunction(x,idbacDirectory$filePath))
}
