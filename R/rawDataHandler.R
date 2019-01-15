
function(chosenDir, 
         filetype,
         msconvertPath = "",
         spectraConversion = spectraConversion(),
         excel,
         tempDir){

  
   
  

if(isolate(chosenDir == "convert_mzml_nav")){
  
  paths <- list.files(mzmlRawFilesLocation(),
                      recursive = TRUE,
                      full.names = TRUE,
                      pattern = ".mz") 
  
  validate(need(length(paths) > 0), "Didn't find any mzML or mzXML files here.")
  
  names(paths) <- tools::file_path_sans_ext(base::basename(paths))

}

  
  
  
  
  
  
  
  if(isolate(chosenDir == "convert_bruker_nav")){
    
    col <- brukerDataNamesPaths(chosenDir, excel)
    tempNames <- tempfile(pattern = rep("", length(col)), 
                          tmpdir = tempDir,
                          fileext = ".mzMl")
    
    # normalize paths  
    mzfilePaths <- lapply(col, shQuote)
    
    tempNames <- normalizePath(tempNames, winslash = "\\", mustWork = FALSE)
    
    tempdirname <- dirname(tempNames)[[1]]
    tempdirname <- shQuote(tempdirname)
    
    msconvertLocation <- IDBacApp::findMSconvert(msconvertPath)
    msconvertLocation <- normalizePath(msconvertLocation, winslash = "\\", mustWork = FALSE)
    
    msconvertLocation <- shQuote(msconvertLocation)
    
    
    #Command-line MSConvert, converts from proprietary vendor data to mzML
    msconvertCmdLineCommands <- lapply(seq_along(mzfilePaths), 
                                       function(x){
                                         
                                         (paste0(msconvertLocation,
                                                        " ",
                                                        paste0(mzfilePaths[[x]],collapse = "",sep=" "),
                                                        " --noindex --mzML --merge -z  --32",
                                                        " --outdir ", tempdirnames ,
                                                        " --outfile ", tempNames))
                                         
                                       })
    
    
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

  
  
  
    
    
    
    
  
  
  


}