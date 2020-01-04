
#' process_mzml
#'
#' @param mzFilePaths file paths of the mzml files
#' @param sampleIds sampleIds that are in the same order as the paths
#' @param idbac_pool idbac single pool, not a list of pools
#' @param acquisitionInfo acquisitionInfo (currently only used when converting from Bruker raw data)
#'
#' @return Nothing direct, creates a sqlite database
#' @export
#'
process_mzml <- function(mzFilePaths,
                         sampleIds,
                         idbac_pool,
                         acquisitionInfo){
  
  
#TODO: fix idbac by moving this outside   
  #idbac_pool <- IDBacApp::createNewSQLITEdb(newExperimentName = newExperimentName,
   #                                         sqlDirectory = sqlDirectory)[[1]]
  
  progLength <- base::length(mzFilePaths)
  
  # withProgress doesn't currently work outside shiny
  if (!is.null(shiny::getDefaultReactiveDomain())) { 
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
                                                         userDBCon = idbac_pool,
                                                         acquisitionInfo = acquisitionInfo[[i]]) # pool connection
                   }
                 })
    
  } else {
    for (i in base::seq_along(mzFilePaths)) {
      
      base::message('Processing in progress...')
      base::message(glue::glue('Sample: {sampleIds[[i]]}; {i} of {progLength}'))
      
      IDBacApp::spectraProcessingFunction(rawDataFilePath = mzFilePaths[[i]],
                                          sampleID = sampleIds[[i]],
                                          userDBCon = idbac_pool,
                                          acquisitionInfo = acquisitionInfo[[i]]) # pool connection
    }
  }
}

