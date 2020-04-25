
#' db_from_mzml
#'
#' @param mzFilePaths file paths of the mzml files
#' @param sampleIds sampleIds that are in the same order as the paths
#' @param idbacPool idbac single pool, not a list of pools
#' @param acquisitionInfo acquisitionInfo (currently only used when converting from Bruker raw data)
#' @param ... pass methods to MALDIquant
#' 
#' @return Nothing direct, creates a sqlite database
#' @export
#'
db_from_mzml <- function(mzFilePaths,
                         sampleIds,
                         idbacPool,
                         acquisitionInfo,
                         ...){
  
  
  
  
  sql_fill_version_table(pool = idbacPool)
  sql_fill_locale_table(pool = idbacPool)
  
  progLength <- base::length(mzFilePaths)
  
  # withProgress doesn't currently work outside shiny
  if (!is.null(shiny::getDefaultReactiveDomain())) { 
    withProgress(message = 'Processing in progress',
                 value = 0,
                 max = progLength, {
                   
                   for (i in base::seq_along(mzFilePaths)) {
                     setProgress(value = i,
                                 message = 'Processing in progress',
                                 detail = glue::glue(" \n Sample: <br> {sampleIds[[i]]},
                                                   {i} of {progLength}"),
                                 session = getDefaultReactiveDomain())
                     
                     spectraProcessingFunction(rawDataFilePath = mzFilePaths[[i]],
                                               sampleID = sampleIds[[i]],
                                               pool = idbacPool,
                                               acquisitionInfo = acquisitionInfo[[i]],
                                               ...)
                   }
                 })
    
  } else {
    for (i in base::seq_along(mzFilePaths)) {
      
      base::message('Processing in progress...')
      base::message(glue::glue('Sample: {sampleIds[[i]]}; {i} of {progLength}'))
      
      spectraProcessingFunction(rawDataFilePath = mzFilePaths[[i]],
                                sampleID = sampleIds[[i]],
                                pool = idbacPool,
                                acquisitionInfo = acquisitionInfo[[i]],
                                ...)
    }
  }
}

