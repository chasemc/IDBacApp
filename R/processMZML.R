
#' processMZML
#'
#' @param mzFilePaths file paths of the mzml files
#' @param sampleIds sampleIds that are in the same order as the paths
#' @param sqlDirectory sqlDirectory 
#' @param newExperimentName newExperimentName 
#'
#' @return Nothing direct, creates a sqlite database
#' @export
#'
processMZML <- function(mzFilePaths,
                        sampleIds,
                        sqlDirectory,
                        newExperimentName){
  
  
  # validate(need(length(mzFilePaths) == length(sampleIds), 
  #               "Temp mzML files and sample ID lengths don't match."
  # ))
  # 
  lengthProgress <- length(mzFilePaths)
  
  userDB <- IDBacApp::createNewSQLITEdb(newExperimentName = newExperimentName,
                                        sqlDirectory = sqlDirectory)[[1]]
  userDB <- pool::poolCheckout(userDB)
  
  progLength <- base::length(mzFilePaths)
  withProgress(message = 'Processing in progress',
               value = 0,
               max = progLength, {
                 
                 for (i in base::seq_along(mzFilePaths)) {
                   setProgress(value = i,
                               message = 'Processing in progress',
                               detail = glue::glue(" \n Sample: {sampleIds[[i]]},
                                                   {i} of {progLength}"),
                               session = getDefaultReactiveDomain())
                   
                   IDBacApp::spectraProcessingFunction(rawDataFilePath = mzFilePaths[[i]],
                                                       sampleID = sampleIds[[i]],
                                                       userDBCon = userDB) # pool connection
                 }
                
                 
               })
  pool::poolReturn(userDB)
  
  
}

