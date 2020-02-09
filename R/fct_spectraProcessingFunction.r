#' Create IDBac SQLite database 
#' Should work for mzML, mzXML, mgf, and txt data input
#' 
#'  
#' @param rawDataFilePath filepath of the data
#' @param sampleID the sample ID to be read and added to the database
#' @param userDBCon database connection (checked out pool)
#' @param acquisitionInfo acquisitionInfo (currently only used when converting from Bruker raw data)
#'
#' @return the peak list modifed by binning then subtractng the matrix sample,
#' 
spectraProcessingFunction <- function(rawDataFilePath,
                                      sampleID,
                                      userDBCon, 
                                      acquisitionInfo){
  
  sampleID <- trimws(sampleID, 
                    which = c("both"))
    
  # If sample ID doesn't exist, create it in table
  # TODO: userprompt with option to change ID
  createMetaSQL(sampleID = sampleID,
                          userDBCon = userDBCon)
  
  
  # Create xml table -------------------------------------------------------

  # Make connection to mzML file
  mzML_con <- mzR::openMSfile(rawDataFilePath,
                              backend = "pwiz")
  
  XMLinfo <- createXMLSQL(rawDataFilePath = rawDataFilePath,
                                    userDBCon = userDBCon,
                                    mzML_con = mzML_con)
  
  
 
  
  createSpectraSQL(mzML_con = mzML_con,
                             userDBCon = userDBCon,
                             sampleID = sampleID,
                             XMLinfo = XMLinfo, 
                             acquisitionInfo = acquisitionInfo)
}













