
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
  
  
  rawDataFilePath <<- rawDataFilePath
  # # Find acquisitionInfo from mzML file
  #acquisitionInfo <- IDBacApp::findAcquisitionInfo(rawDataFilePath,
   #                                               instInfo$manufacturer)
  
  # if ("Instrument_MetaFile" %in% ls(acquisitionInfo)) { 
  #   sqlDataFrame$XML$Instrument_MetaFile <- IDBacApp::serial(acquisitionInfo$Instrument_MetaFile)
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
#' @param smallRangeEnd end of mass region for small mol, if m/z above this- will be classified as "protein" spectrum
#' @param acquisitionInfo acquisitionInfo (currently only used when converting from Bruker raw data)
#'
#' @return NA
#' @export
#'
createSpectraSQL <- function(mzML_con, 
                             scanNumber,
                             userDBCon,
                             sampleID,
                             XMLinfo,
                             smallRangeEnd = 6000,
                             acquisitionInfo){
  
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
                                          userDBCon = userDBCon,
                                          acquisitionInfo = acquisitionInfo[!smallIndex],
                                          sampleID = sampleID)
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
                                          userDBCon = userDBCon,
                                          acquisitionInfo = acquisitionInfo[!smallIndex],
                                          sampleID = sampleID)
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
#' @param acquisitionInfo acquisitionInfo
#' @param sampleID sampleID
#'
#' @return nothing, writes to database
#' @export
#'
insertIntoIndividualSpectra <- function(env,
                                        XMLinfo,
                                        userDBCon,
                                        acquisitionInfo = NULL,
                                        sampleID){
  
  
  env <<- env
  XMLinfo <<- XMLinfo
  userDBCon <<- userDBCon
  acquisitionInfo2 <<- acquisitionInfo
  sampleID <<- sampleID
  
  
  
  
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
                                  'peakMatrix',
                                  'spectrumIntensity',
                                  'maxMass',
                                  'minMass',
                                  'ignore',
                                  'number',
                                  'timeDelay',
                                  'timeDelta',
                                  'calibrationConstants',
                                  'v1tofCalibration',
                                  'dataType',
                                  'dataSystem',
                                  'spectrometerType',
                                  'inlet',
                                  'ionizationMode',
                                  'acquisitionMethod',
                                  'acquisitionDate',
                                  'acquisitionMode',
                                  'tofMode',
                                  'acquisitionOperatorMode',
                                  'laserAttenuation',
                                  'digitizerType',
                                  'flexControlVersion',
                                  'id',
                                  'instrument',
                                  'instrumentId',
                                  'instrumentType',
                                  'massError',
                                  'laserShots',
                                  'patch',
                                  'path',
                                  'laserRepetition',
                                  'spot',
                                  'spectrumType',
                                  'targetCount',
                                  'targetIdString',
                                  'targetSerialNumber',
                                  'targetTypeNumber')
                                  VALUES ($spectrumMassHash,
                                  $spectrumIntensityHash,
                                  $XMLHash,
                                  $Strain_ID,
                                  $peakMatrix,
                                  $spectrumIntensity,
                                  $maxMass,
                                  $minMass,
                                  $ignore,
                                  $number,
                                  $timeDelay,
                                  $timeDelta,
                                  $calibrationConstants,
                                  $v1tofCalibration,
                                  $dataType,
                                  $dataSystem,
                                  $spectrometerType,
                                  $inlet,
                                  $ionizationMode,
                                  $acquisitionMethod,
                                  $acquisitionDate,
                                  $acquisitionMode,
                                  $tofMode,
                                  $acquisitionOperatorMode,
                                  $laserAttenuation,
                                  $digitizerType,
                                  $flexControlVersion,
                                  $id,
                                  $instrument,
                                  $instrumentId,
                                  $instrumentType,
                                  $massError,
                                  $laserShots,
                                  $patch,
                                  $path,
                                  $laserRepetition,
                                  $spot,
                                  $spectrumType,
                                  $targetCount,
                                  $targetIdString,
                                  $targetSerialNumber,
                                  $targetTypeNumber
                                  );"
    )
    
    
   

    ignore <- rep(0, times = temp[[1]])
    sampleID <- rep(sampleID[[1]], times = temp[[1]])
    mzMLHash <- rep(XMLinfo$mzMLHash, times = temp[[1]])
    
    
      
    a <- c('number',
           'timeDelay',
           'timeDelta',
           'calibrationConstants',
           'v1tofCalibration',
           'dataType',
           'dataSystem',
           'spectrometerType',
           'inlet',
           'ionizationMode',
           'acquisitionMethod',
           'acquisitionDate',
           'acquisitionMode',
           'tofMode',
           'acquisitionOperatorMode',
           'laserAttenuation',
           'digitizerType',
           'flexControlVersion',
           'id',
           'instrument',
           'instrumentId',
           'instrumentType',
           'massError',
           'laserShots',
           'patch',
           'path',
           'laserRepetition',
           'spot',
           'spectrumType',
           'targetCount',
           'targetIdString',
           'targetSerialNumber',
           'targetTypeNumber')
      
    
    if (!is.null(acquisitionInfo) || !is.list(acquisitionInfo)) {
     
      acquisitionInfo <- list()
       
    } else {
    
      # Account for missing fields
    acquisitionInfo <- lapply(acquisitionInfo, function(acquisitionInfo){
      
      acquisitionInfo[which(lengths(acquisitionInfo) == 0)] <- NA
      
      # if length > 1, serialize to json
      acquisitionInfo[which(lengths(acquisitionInfo) > 1)] <- lapply(acquisitionInfo[which(lengths(acquisitionInfo) > 1)], 
                                                                     jsonlite::serializeJSON)
      
      
      w <- a[!a %in% names(acquisitionInfo)]
      ww <- as.list(w)
      names(ww) <- w
      ww[] <- NA
      
      acquisitionInfo <- c(acquisitionInfo,
                           ww)
        do.call(rbind.data.frame,
                   list(acquisitionInfo,
                        stringsAsFactors = FALSE)
        )
      
    })
    
    
    
    acquisitionInfo <- do.call(rbind, acquisitionInfo)
    
    acquisitionInfo <- acquisitionInfo[ ,names(acquisitionInfo) %in% a]
      
    }
    
    
    
      DBI::dbBind(query, 
                  c(
                    list(spectrumMassHash = env$spectrumMassHash,
                         spectrumIntensityHash = env$spectrumIntensityHash,
                         XMLHash = mzMLHash,
                         Strain_ID = sampleID,
                         peakMatrix = env$peakMatrix,
                         spectrumIntensity = env$spectrumIntensity,
                         minMass = env$minMass,
                         maxMass = env$maxMass,
                         ignore = ignore
                    ),
                    acquisitionInfo
                    
                         
                    ))
    
    
    
    DBI::dbClearResult(query)
    
  }
}