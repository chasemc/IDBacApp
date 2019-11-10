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
#' @export
spectraProcessingFunction <- function(rawDataFilePath,
                                      sampleID,
                                      userDBCon, 
                                      acquisitionInfo){
  
  IDBacApp::poolToCon(userDBCon)
  
  
  
  sampleID <- IDBacApp::cleanWSpace(sampleID)
  
  
  # Create version and metadata SQL tables ----------------------------------
  
  
  # Isn't currently used, but here to help future-proof
  IDBacApp::sqlCreate_version(userDBCon = userDBCon)
  #----
  
  # Isn't currently used, but here to help future-proof
  # Inserts current locale info 
  IDBacApp::insertLocale(userDBCon = userDBCon)
  
  # If sample ID doesn't exist, create it in table
  # TODO: userprompt with option to change ID
  IDBacApp::createMetaSQL(sampleID = sampleID,
                          userDBCon = userDBCon)
  
  
  
  # Create XML table --------------------------------------------------------
  
  
  
  # Make connection to mzML file
  mzML_con <- mzR::openMSfile(rawDataFilePath,
                              backend = "pwiz")
  
  XMLinfo <- IDBacApp::createXMLSQL(rawDataFilePath = rawDataFilePath,
                                    userDBCon = userDBCon,
                                    mzML_con = mzML_con)
  
  
  # Get number of spectra contained in mzML
  scanNumber <- nrow(mzR::header(mzML_con))
  

  
  IDBacApp::createSpectraSQL(mzML_con = mzML_con,
                             scanNumber = scanNumber,
                             userDBCon = userDBCon,
                             sampleID = sampleID,
                             XMLinfo = XMLinfo, 
                             acquisitionInfo = acquisitionInfo)
}













