
convertToMzml <- function(mzmlRawFileDirectory,
                          mzmlRawFilesLocation,
                          pwizFolderLocation,
                          outDir){

if(!is.null(mzmlRawFileDirectory)){
  
  
  mzFileInput <- normalizePath(mzmlRawFilesLocation, winslash = "/" )
  
  fullZ <- NULL
  
  fullZ$UserInput.x <- basename(tools::file_path_sans_ext(mzFileInput))
  fullZ$UserInput.y <- mzFileInput
  
  fullZ <- do.call(cbind.data.frame, fullZ)
  fullZ <- split(fullZ, 1:nrow(fullZ))
  
}else{
  
  fullZ <- spectraConversion
}

fullZ <- lapply(fullZ,
                function(x){
                  cbind(x,
                        tempFile = basename(tempfile(pattern = "", 
                                                     tmpdir = tempMZ,
                                                     fileext = "")
                        ), stringsAsFactors = F)
                }
)


# fullZ$UserInput.x = sample name
# fullZ$UserInput.y = file locations




#Command-line MSConvert, converts from proprietary vendor data to open mzML
msconvertCmdLineCommands <<- lapply(fullZ, function(x){
  #Finds the msconvert.exe program which is located the in pwiz folder which is two folders up ("..\\..\\") from the directory in which the IDBac shiny app initiates from
  paste0(shQuote(file.path(pwizFolderLocation,
                           "msconvert.exe")),
         # sets up the command to pass to MSConvert in commandline, with variables for the input files (x$UserInput.y) and for where the newly created mzML files will be saved
         " ",
         paste0(shQuote(x$UserInput.y), 
                collapse = "",
                sep=" "),
         # "--noindex --mzML --merge -z",
         "--noindex --mzML --merge -z  --32",
         " -o ",
         shQuote(outDir),
         " --outfile ",
         shQuote(paste0(x$tempFile, ".mzML"))
  )
}
)

functionTOrunMSCONVERTonCMDline<-function(x){
  system(command = as.character(x))
}

popup1()

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
