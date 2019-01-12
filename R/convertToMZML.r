
#' Convert to mzML
#'
#' @param mzmlRawFileDirectory NA
#' @param mzmlRawFilesLocation NA 
#' @param msconvertLocation  NA
#' @param outDir  NA
#' @param spectraConversion  NA
#'
#' @return NA
#' @export
#'
#' @examples NA
convertToMzml <- function(mzmlRawFileDirectory,
                          mzmlRawFilesLocation,
                          msconvertLocation,
                          outDir,
                          spectraConversion){

if(!is.null(mzmlRawFileDirectory)){
  
  # vector of filepaths
  tempNames <- normalizePath(mzmlRawFilesLocation, winslash = "/" )
  
  # vector of file names, no extensions
  filenames <- basename(tools::file_path_sans_ext(tempNames))

}else{
  # Get names from excel
  fullZ <- spectraConversion


  tempNames <- tempfile(pattern = rep("", length(filenames)), 
                               tmpdir = outDir,
                               fileext = ".mzMl")


# normalize paths  
mzfilePaths <- shQuote(mzfilePaths)
tempNames <- normalizePath(tempNames, winslash = "/", mustWork = FALSE)

tempdirnames <- dirname(tempNames)
tempdirnames <- shQuote(tempdirnames)

msconvertLocation <- normalizePath(msconvertLocation, winslash = "/", mustWork = FALSE)
msconvertLocation <- shQuote(msconvertLocation)


#Command-line MSConvert, converts from proprietary vendor data to mzML
msconvertCmdLineCommands <- as.list(paste0(msconvertLocation,
         # sets up the command to pass to MSConvert in commandline, with variables for the input files (x$UserInput.y) and for where the newly created mzML files will be saved
         " ",
         mzfilePaths,
         " --noindex --mzML --merge -z  --32",
         " --outdir ", tempdirnames ,
         " --outfile ", tempNames)
         )


functionTOrunMSCONVERTonCMDline<-function(x){
  system(command = x)
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

}


return(list(paths = tempNames,
            names = filenames)
       )



#Single process with sapply instead of parsapply
#sapply(fileList,function(x)spectraProcessingFunction(x,idbacDirectory$filePath))
}
