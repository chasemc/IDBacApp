
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
                                $Instrument_MetaFile);")
  
  DBI::dbBind(query, list(XMLHash = mzMLHash,
                          XML = list(xmlFile),
                          manufacturer = instInfo$manufacturer[[1]],
                          model = instInfo$model[[1]],
                          ionization = instInfo$ionisation[[1]],
                          analyzer = instInfo$analyzer[[1]],
                          detector = instInfo$detector[[1]],
                          Instrument_MetaFile = "Unkown"))
  
  DBI::dbClearResult(query)
  
  
  
  return(list(mzMLHash = mzMLHash,
              mzMLInfo = instInfo))
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
  
  
   mzML_con<<-mzML_con
   scanNumber<<-scanNumber
   userDBCon<<-userDBCon
   sampleID<<-sampleID
   XMLinfo<<-XMLinfo
   rawDataFilePath<<-rawDataFilePath
   smallRangeEnd <<-6000
  
  
  spectraImport <- mzR::peaks(mzML_con)
  
  spectraImport <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport)
  
  
  # logical vector of maximum masses of mass vectors. True = small mol, False = protein
  smallIndex <- unlist(lapply(spectraImport, function(x) max(x@mass)))
  smallIndex <- smallIndex < smallRangeEnd
  
  
  

# Create tables in DB if they don't exist ---------------------------------

  if (!DBI::dbExistsTable(userDBCon, "IndividualSpectra")) {
    IDBacApp::sql_CreateIndividualSpectra(userDBCon)
  }  
  if (!DBI::dbExistsTable(userDBCon, "massTable")) {
    IDBacApp::sql_CreatemassTable(userDBCon)
  }
  
  # Small mol spectra -------------------------------------------------------
  
  if (any(smallIndex)) { 
    env <- IDBacApp::processXMLIndSpectra(spectraImport = spectraImport,
                                          smallOrProtein = "small",
                                          index = smallIndex)
    IDBacApp::insertIntoIndividualSpectra(env = env,
                                          XMLinfo = XMLinfo,
                                          userDBCon = userDBCon)
    IDBacApp::insertIntoMassTable(env = env,
                                  userDBCon = userDBCon)
  }
  # Protein Spectra ---------------------------------------------------------
  
  if (any(!smallIndex)) {
    
    env <- IDBacApp::processXMLIndSpectra(spectraImport = spectraImport,
                                          smallOrProtein = "protein",
                                          index = !smallIndex)
    IDBacApp::insertIntoIndividualSpectra(env = env,
                                          XMLinfo = XMLinfo,
                                          userDBCon = userDBCon)
    IDBacApp::insertIntoMassTable(env = env,
                                  userDBCon = userDBCon)
  }
  
  
}






#' Write massTable data to SQLite
#'
#' @param env environment 
#' @param userDBCon checked database connection
#'
#' @return nothing, writes to database
#' @export
#'
insertIntoMassTable <- function(env,
                                userDBCon){
  
  if (length(env$spectrumMassHash) != length(env$massVector)) {
    stop("Error in IDBacApp::insertIntoMassTable(): IDBacApp::processXMLIndSpectra() provided
                    spectrumMassHash and massVector variables with different lengths")
  } else { 
    query <- DBI::dbSendStatement(userDBCon, 
                                  "INSERT INTO 'massTable'(
                              'spectrumMassHash',
                              'massVector')
                              VALUES (
                              $spectrumMassHash,
                              $massVector);")
    
    DBI::dbBind(query, list(spectrumMassHash = env$spectrumMassHash,
                            massVector = env$massVector)
    )
    
    DBI::dbClearResult(query)
    
  }
}





#' Write individual spectra to SQLite
#'
#' @param env environment 
#' @param XMLinfo xmlinfo
#' @param userDBCon checked database connection
#' @param acquisitonInfo acquisitonInfo
#'
#' @return nothing, writes to database
#' @export
#'
insertIntoIndividualSpectra <- function(env,
                                        XMLinfo,
                                        userDBCon,
                                        acquisitonInfo){
  
  temp <- base::lengths(base::mget(base::ls(env),
                                   envir = as.environment(env))) 
  
  # ensure equal lengths
  if ((sum(temp)/temp[[1]]) != length(temp)) {
    stop(glue::glue("Error in IDBacApp::insertIntoIndividualSpectra(): IDBacApp::processXMLIndSpectra() provided variables of differing lengths: \n ",  
                    paste0(names(temp),"=",temp, collapse = ", ")))
  } else { 
    query <- DBI::dbSendStatement(userDBCon, 
                                  "INSERT INTO 'IndividualSpectra'(
                                  'spectrumMassHash',
                                  'spectrumIntensityHash',
                                  'XMLHash',
                                  'Strain_ID',
                                  'MassError',
                                  'AcquisitionDate',
                                  'peakMatrix',
                                  'spectrumIntensity',
                                  'maxMass',
                                  'minMass',
                                  'ignore')
                                  VALUES ($spectrumMassHash,
                                  $spectrumIntensityHash,
                                  $XMLHash,
                                  $Strain_ID,
                                  $MassError,
                                  $AcquisitionDate,
                                  $peakMatrix,
                                  $spectrumIntensity,
                                  $minMass,
                                  $maxMass,
                                  $ignore
                                  );"
    )
    
    
    if (is.null(XMLinfo$mzMLInfo$AcquisitionDate)) {
      XMLinfo$mzMLInfo$AcquisitionDate <- NA
    }  
    # if (is.null(acquisitonInfo$MassError)) {
    #   acquisitonInfo$MassError <- NA
    # } 
    
    mzMLHash <- rep(XMLinfo$mzMLHash, times = temp[[1]])
    acquisitionDate <- rep(XMLinfo$mzMLInfo$AcquisitionDate, times = temp[[1]])
    MassError <- rep(NA, times = temp[[1]])
    ignore <- rep(0, times = temp[[1]])
    sampleID <- rep(sampleID[[1]], times = temp[[1]])
    
    DBI::dbBind(query, list(spectrumMassHash = env$spectrumMassHash,
                            spectrumIntensityHash = env$spectrumIntensityHash,
                            XMLHash = mzMLHash,
                            Strain_ID = sampleID,
                            MassError = MassError,
                            AcquisitionDate = acquisitionDate,
                            peakMatrix = env$peakMatrix,
                            spectrumIntensity = env$spectrumIntensity,
                            minMass = env$minMass,
                            maxMass = env$maxMass,
                            ignore = ignore
    ))
    
    
    DBI::dbClearResult(query)
    
  }
}