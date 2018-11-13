excelMaptoPlateLocation <- function(rawORreanalyze,
                                    excelFileLocation, 
                                    rawFilesLocation,
                                    multipleMaldiRawFileLocation){


if(rawORreanalyze == 1){
  # When only analyzing one MALDI plate this handles finding the raw data directories and the excel map
  # excelMap is a dataframe (it's "Sheet1" of the excel template)
  excelTable <- as.data.frame(readxl::read_excel(paste0(excelFileLocation), 2))
  
  # excelTable takes the sample location and name from excelTable, and also converts the location to the same name-format as Bruker (A1 -> 0_A1)
  excelTable <- cbind.data.frame(paste0("0_", 
                                        excelTable$Key),
                                 excelTable$Value)
  
  # List the raw data files (for Bruker MALDI files this means pointing to a directory, not an individual file)
  fullZ <- list.dirs(list.dirs(rawFilesLocation, 
                               recursive = FALSE), 
                     recursive = FALSE)
  
  # Get folder name from fullZ for merging with excel table names
  fullZ <- cbind.data.frame(fullZ, 
                            unlist(lapply(fullZ, 
                                          function(x) strsplit(x, "/")[[1]][[3]])))
  colnames(fullZ) <- c("UserInput", "ExcelCell")
  colnames(excelTable) <- c("ExcelCell", "UserInput")
  
  # Merge to connect filenames in excel sheet to file paths
  fullZ <- merge(excelTable,
                 fullZ,
                 by = c("ExcelCell"))
  fullZ[,3] <- normalizePath(as.character(fullZ[ , 3]))
  
} else if(rawORreanalyze == 3) {
  
  # When analyzing more han one MALDI plate this handles finding the raw data directories and the excel map
  mainDirectory <- list.dirs(multipleMaldiRawFileLocation,
                             recursive = F)
  lapped <- lapply(mainDirectory,
                   function(x) list.files(x, 
                                          recursive = F, 
                                          full.names = T))
  collectfullZ <- NULL
  
  # For annotation, look at the single-plate conversion above, the below is basically the same, but iterates over multiple plates, each plate must reside in its own directory.
  for (i in 1:length(lapped)){
    
    excelTable <- as.data.frame(read_excel(lapped[[i]][grep(".xls", lapped[[i]])], 2))
    excelTable <- cbind.data.frame(paste0("0_", 
                                          excelTable$Key),
                                   excelTable$Value)
    fullZ <- list.dirs(lapped[[i]],
                       recursive = F)
    fullZ <- cbind.data.frame(fullZ,
                              unlist(lapply(fullZ, 
                                            function(x) strsplit(x, "/")[[1]][[4]])))
    colnames(fullZ) <- c("UserInput", "ExcelCell")
    colnames(excelTable) <- c("ExcelCell", "UserInput")
    fullZ <- merge(excelTable,
                   fullZ,
                   by = c("ExcelCell"))
    fullZ[ , 3] <- normalizePath(as.character(fullZ[ , 3]))
    collectfullZ <- c(collectfullZ, list(fullZ))
    
  }
  
  fullZ <- plyr::ldply(collectfullZ, data.frame)  #TODO Look into converting to base::
  
}

fullZ <- plyr::dlply(fullZ, .(UserInput.x))
# Allow spaces in filepath

for (i in 1:length(fullZ)){
  fullZ[[i]]$UserInput.y <- shQuote(fullZ[[i]]$UserInput.y)
}
# return fullz to the "spectraConversion" reactive variable, this is  is a named list, where each element represents a sample and the element name is the sample name;
# contents of each element are file paths to the raw data for that sample
# This will be used by the spectra conversion observe function/event
return(fullZ)


}