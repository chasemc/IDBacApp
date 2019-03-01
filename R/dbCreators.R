
# version table -----------------------------------------------------------



#' Create version table 
#'
#' @param userDBCon sqlite connection
#'
#' @return NA
#' @export
#'
sqlCreate_version <- function(userDBCon) {
  if (!DBI::dbExistsTable(userDBCon, "version")) {
    
    # Add version table
    DBI::dbWriteTable(conn = userDBCon,
                      name = "version", # SQLite table to insert into
                      IDBacApp::sqlTableArchitecture(numberScans = 1)$version, # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
  }
}



# metadata table ----------------------------------------------------------



#' createMetaSQL
#'
#' @param sampleID NA
#' @param userDBCon NA
#'
#' @return NA
#' @export
#'

createMetaSQL <- function(sampleID,
                          userDBCon){
  
  if (!DBI::dbExistsTable(userDBCon, "metaData")) {
    IDBacApp::sql_CreatemetaData(userDBCon)
  }  
  
  query <- DBI::dbSendStatement(userDBCon, 
                                "INSERT INTO 'metaData' (
                                'Strain_ID')
                                 VALUES (?)")
  
  DBI::dbBind(query, list(sampleID))
  
  DBI::dbClearResult(query)
  
}



# XML table ---------------------------------------------------------------


#' createXMLSQL
#'
#' @param rawDataFilePath NA
#' @param sampleID NA
#' @param userDBCon NA
#' @param mzML_con NA
#'
#' @return NA
#' @export
#'

createXMLSQL <- function(rawDataFilePath,
                         userDBCon,
                         mzML_con){
  
  xmlFile <- IDBacApp::serializeXML(rawDataFilePath)
  
  mzMLHash <- IDBacApp::hashR(xmlFile)
  
  if (!DBI::dbExistsTable(userDBCon, "XML")) {
  IDBacApp::sql_CreatexmlTable(userDBCon)
  }
  
  
  # Get instrument Info
  instInfo <- mzR::instrumentInfo(mzML_con)

  # # Find acquisitonInfo from mzML file
  # acquisitonInfo <- IDBacApp::findAcquisitionInfo(rawDataFilePath,
  #                                                 instInfo$manufacturer)
  # 
  # if ("Instrument_MetaFile" %in% ls(acquisitonInfo)) { 
  #   sqlDataFrame$XML$Instrument_MetaFile <- IDBacApp::serial(acquisitonInfo$Instrument_MetaFile)
  # }
  
  
  query <- DBI::dbSendStatement(userDBCon, 
                                "INSERT INTO 'XML'(
                                'XMLHash',
                                'XML',
                                'manufacturer',
                                'model',
                                'ionization',
                                'analyzer',
                                'detector',
                                'Instrument_MetaFile')
                                VALUES ($XMLHash,
                                $XML,
                                $manufacturer,
                                $model,
                                $ionization,
                                $analyzer,
                                $detector,
                                $Instrument_MetaFile)")
  
  DBI::dbBind(query, list(XMLHash = mzMLHash,
                          XML = blob::as.blob(xmlFile),
                          manufacturer = instInfo$manufacturer[[1]],
                          model = instInfo$model[[1]],
                          ionization = instInfo$ionisation[[1]],
                          analyzer = instInfo$analyzer[[1]],
                          detector = instInfo$detector[[1]],
                          Instrument_MetaFile = "Unkown"))
  
  DBI::dbClearResult(query)
  
  
  
  return(list(mzMLHash = mzMLHash,
              mzMLInfo = NULL))
}
