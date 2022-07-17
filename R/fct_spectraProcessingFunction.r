#' Create IDBac SQLite database 
#' Should work for mzML, mzXML, mgf, and txt data input
#' 
#'  
#' @param rawDataFilePath filepath of the data
#' @param sampleID the sample ID to be read and added to the database
#' @param pool database connection (checked out pool)
#' @param acquisitionInfo acquisitionInfo (currently only used when converting from Bruker raw data)
#' @param ... advanced arguments for MALDIquant, see [IDBacApp::processSmallMolSpectra()] and/or [IDBacApp::processProteinSpectra()]
#' 
#' @return the peak list modifed by binning then subtractng the matrix sample,
#' 
spectraProcessingFunction <- function(rawDataFilePath,
                                      sampleID,
                                      pool, 
                                      acquisitionInfo,
                                      ...){
  
  
  if (!length(sampleID) > 0) {
    stop("sampleID lenghth must be > 0 ")
  }
  
  sampleID <- trimws(sampleID, 
                     which = c("both"))
  
  # If sample ID doesn't exist, create it in table
  # TODO: userprompt with option to change ID
  createMetaSQL(sampleID = sampleID,
                pool = pool)
  
  
  # Create xml table -------------------------------------------------------
  
  # Make connection to mzML file
  mzML_con <- mzR::openMSfile(rawDataFilePath,
                              backend = "pwiz")
  
  XMLinfo <- createXMLSQL(rawDataFilePath = rawDataFilePath,
                          pool = pool,
                          mzML_con = mzML_con)
  
  
  
  
  createSpectraSQL(mzML_con = mzML_con,
                   pool = pool,
                   sampleID = sampleID,
                   XMLinfo = XMLinfo, 
                   acquisitionInfo = acquisitionInfo,
                   ...)
}













