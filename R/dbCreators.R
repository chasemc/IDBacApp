
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



#' createSpectraSQL
#'
#' @param mzML_con NA
#' @param scanNumber NA
#' @param userDBCon NA
#' @param sampleID NA
#' @param XMLinfo NA
#' @param rawDataFilePath NA
#' @param smallRangeEnd end of mass region for small mol, if m/z above this- will be classified as "protein" spectrum
#'
#' @return NA
#' @export
#'

createSpectraSQL <- function(mzML_con, 
                             scanNumber,
                             userDBCon,
                             sampleID,
                             XMLinfo,
                             rawDataFilePath,
                             smallRangeEnd = 6000){
  
  
  # mzML_con<<-mzML_con
  # scanNumber<<-scanNumber
  # userDBCon<<-userDBCon
  # sampleID<<-sampleID
  # XMLinfo<<-XMLinfo
  # rawDataFilePath<<-rawDataFilePath
  # smallRangeEnd <<-6000
  
  
  spectraImport <- mzR::peaks(mzML_con)
  
  spectraImport <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport)

 
  # logical vector of maximum masses of mass vectors. True = small mol, False = protein
  smallIndex <- unlist(lapply(spectraImport, function(x) max(x@mass)))
  smallIndex <- smallIndex < smallRangeEnd
  
  # Small mol spectra -------------------------------------------------------
  
  if (any(smallIndex)) { 
    
    spectrumMass <- lapply(spectraImport[smallIndex], 
                                   function(x){
                                     x <- IDBacApp::serial(x@mass)
                                     IDBacApp::chartoRawtoCompressed(x,
                                                                     compression = 100)
                                   })
    
    # List of whole-specctrum hashes
    spectrumMassHash <- lapply(spectrumMass, 
                                       function(x){
                                         IDBacApp::hashR(x)
                                       })
    
    spectrumIntensity <- lapply(spectraImport[smallIndex], 
                                             function(x){
                                               x <- IDBacApp::serial(x@intensity)
                                               IDBacApp::chartoRawtoCompressed(x,
                                                                               compression = 100)
                                             })
    
    spectrumIntensityHash <- lapply(spectrumIntensity, 
                                        function(x){
                                          IDBacApp::hashR(x)
                                        })
 
    
    peaks <- IDBacApp::processSmallMolSpectra(spectraImport[smallIndex])
    
    
    
    
    
    
    
    
  }  
  
  # Protein Spectra ---------------------------------------------------------
  
  if (any(!smallIndex)) {
    
    #List of serialized intensity vectors 
    spectrumIntensityHash <- IDBacApp::mzRpeakSerializer(spectraImport[!smallIndex], 
                                                                                                     column = "intensity")
    # vector of hashes
    spectrumIntensityHash <- unlist(lapply(spectrumIntensityHash,
                                           function(x) {
                                             IDBacApp::hashR(x)
                                           })
                                    )
    
    
    
    peaks <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport[!smallIndex])
    peaks <- IDBacApp::processProteinSpectra(peaks)
    
    sqlDataFrame$IndividualSpectra$proteinPeaksMass[!smallIndex] <- lapply(peaks, function(x) x@mass)
    sqlDataFrame$IndividualSpectra$proteinPeaksMass[!smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksMass[!smallIndex], 
                                                                           function(x){
                                                                             IDBacApp::compress(IDBacApp::serial(x))
                                                                           })
    
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex] <- lapply(peaks, function(x) x@intensity)
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex], 
                                                                                function(x){
                                                                                  IDBacApp::compress(IDBacApp::serial(x))
                                                                                })
    
    sqlDataFrame$IndividualSpectra$proteinPeaksSNR[!smallIndex] <- lapply(peaks, function(x) x@snr)
    sqlDataFrame$IndividualSpectra$proteinPeaksSNR[!smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksSNR[!smallIndex], 
                                                                          function(x){
                                                                            IDBacApp::compress(IDBacApp::serial(x))
                                                                          })
    
    
    
  }
  
  remove(peaks)
  
  
  # List of raw vectors
  binaryMassVector <- IDBacApp::mzRpeakSerializer(spectraImport, column = "mass")
  
  # List of hashes of same length as binaryMassVector
  spectrumMassHash <- lapply(binaryMassVector,
                             function(x) {
                               IDBacApp::hashR(x)
                             })
  
  
  
  
  sqlDataFrame$IndividualSpectra$mzMLHash <- XMLinfo$mzMLHash
  sqlDataFrame$IndividualSpectra$Strain_ID <- sampleID
  
  if ("MassError" %in% ls(XMLinfo$mzMLInfo)) {
    sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[individualSpectrum]]
  }
  sqlDataFrame$IndividualSpectra$AcquisitionDate <- XMLinfo$mzMLInfo$AcquisitionDate
  
  
  
  # Write massTable ---------------------------------------------------------
  
  
  if (!DBI::dbExistsTable(userDBCon, "massTable")) {
    
    sta <- RSQLite::dbSendStatement(userDBCon, sqlDataFrame$massTableSQL)
    RSQLite::dbClearResult(sta)
  } 
  # Write to SQL DB
  DBI::dbWriteTable(conn = userDBCon,
                    name = "massTable", # SQLite table to insert into
                    sqlDataFrame$massTable, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
  
  
  
  
  # Write IndividualSpectra -------------------------------------------------
  
  
  
  
  if (!DBI::dbExistsTable(userDBCon, "IndividualSpectra")) {
    
    sta <- RSQLite::dbSendStatement(userDBCon, sqlDataFrame$IndividualSpectraSQL)
    RSQLite::dbClearResult(sta)
  }
  # Write to SQL DB
  DBI::dbWriteTable(conn = userDBCon,
                    name = "IndividualSpectra", # SQLite table to insert into
                    sqlDataFrame$IndividualSpectra, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
  
  
  
}