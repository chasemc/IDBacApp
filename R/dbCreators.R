
# version table -----------------------------------------------------------



#' Create version table 
#'
#' @param userDBCon sqlite connection
#'
#' @return NA
#' @export
#'
sqlCreate_version <- function(userDBCon) {
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      if (!DBI::dbExistsTable(conn, "version")) {
        
        ver <- cbind.data.frame(IDBacVersion = as.character(packageVersion("IDBacApp")), 
                                rVersion = as.character(IDBacApp::serial(sessionInfo()$R.version)))
        # Add version table
        DBI::dbWriteTable(conn = conn,
                          name = "version", # SQLite table to insert into
                          ver, # Insert single row into DB
                          append = TRUE, # Append to existing table
                          overwrite = FALSE) # Do not overwrite
      }
    })
}


# locale table ------------------------------------------------------------


#' Insert current locale info into sql table 
#'
#' @param userDBCon  database connection
#'
#' @return side effect
#' @export
#'
insertLocale <- function(userDBCon) {
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      if (!DBI::dbExistsTable(conn, "locale")) {
        
        locale <- Sys.getlocale(category = "LC_ALL")
        locale <- as.character(locale)[[1]]
        locale <- as.data.frame(locale)
        
        # Add version table
        DBI::dbWriteTable(conn = conn,
                          name = "locale", # SQLite table to insert into
                          locale, # Insert single row into DB
                          append = TRUE, # Append to existing table
                          overwrite = FALSE) # Do not overwrite
      }
      
    })
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
  
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      if (!DBI::dbExistsTable(conn, "metaData")) {
        IDBacApp::sql_CreatemetaData(conn)
      }  
      
      query <- DBI::dbSendStatement(conn, 
                                    "INSERT INTO 'metaData' (
                                'strain_id')
                                 VALUES (?)")
      
      DBI::dbBind(query, list(sampleID))
      
      DBI::dbClearResult(query)
      
    })
}  


# xml table ---------------------------------------------------------------


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
  
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      xmlFile <- IDBacApp::serializeXML(rawDataFilePath)
      
      mzMLHash <- IDBacApp::hashR(xmlFile)
      
      if (!DBI::dbExistsTable(conn, "xml")) {
        IDBacApp::sql_CreatexmlTable(conn)
      }
      
      
      # Get instrument Info
      instInfo <- mzR::instrumentInfo(mzML_con)
      
      
      # # Find acquisitionInfo from mzML file
      #acquisitionInfo <- IDBacApp::findAcquisitionInfo(rawDataFilePath,
      #                                               instInfo$manufacturer)
      
      # if ("instrument_metafile" %in% ls(acquisitionInfo)) { 
      #   sqlDataFrame$xml$instrument_metafile <- IDBacApp::serial(acquisitionInfo$instrument_metafile)
      # }
      
      
      query <- DBI::dbSendStatement(conn, 
                                    "INSERT INTO 'xml'(
                                'xml_hash',
                                'xml',
                                'manufacturer',
                                'model',
                                'ionization',
                                'analyzer',
                                'detector',
                                'instrument_metafile')
                                VALUES ($xml_hash,
                                $xml,
                                $manufacturer,
                                $model,
                                $ionization,
                                $analyzer,
                                $detector,
                                $instrument_metafile);")
      
      DBI::dbBind(query, list(xml_hash = mzMLHash,
                              xml = list(xmlFile),
                              manufacturer = instInfo$manufacturer[[1]],
                              model = instInfo$model[[1]],
                              ionization = instInfo$ionisation[[1]],
                              analyzer = instInfo$analyzer[[1]],
                              detector = instInfo$detector[[1]],
                              instrument_metafile = "Unkown"))
      
      DBI::dbClearResult(query)
      return(list(mzMLHash = mzMLHash,
                  mzMLInfo = instInfo))
    })
  
  
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
  
  if (!pool::dbExistsTable(userDBCon, "individual_spectra")) {
    pool::poolWithTransaction(
      pool = userDBCon,
      func = function(conn){
        IDBacApp::sql_CreateIndividualSpectra(conn)
      })
  }  
  if (!pool::dbExistsTable(userDBCon, "massTable")) {
    pool::poolWithTransaction(
      pool = userDBCon,
      func = function(conn){
        IDBacApp::sql_CreatemassTable(conn)
      })
  }
  
  # Small mol spectra -------------------------------------------------------
  
  if (any(smallIndex)) { 
    env <- IDBacApp::processXMLIndSpectra(spectraImport = spectraImport,
                                          smallOrProtein = "small",
                                          index = smallIndex)
    
    IDBacApp::insertIntoIndividualSpectra(env = env,
                                          XMLinfo = XMLinfo,
                                          userDBCon = userDBCon,
                                          acquisitionInfo = acquisitionInfo[smallIndex],
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
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      if (length(env$spectrum_mass_hash) != length(env$massVector)) {
        stop("Error in IDBacApp::insertIntoMassTable(): IDBacApp::processXMLIndSpectra() provided
                    spectrum_mass_hash and massVector variables with different lengths")
      } else { 
        query <- DBI::dbSendStatement(conn, 
                                      "INSERT INTO 'massTable'(
                              'spectrum_mass_hash',
                              'massVector')
                              VALUES (
                              $spectrum_mass_hash,
                              $massVector);")
        
        DBI::dbBind(query, list(spectrum_mass_hash = env$spectrum_mass_hash,
                                massVector = env$massVector)
        )
        
        DBI::dbClearResult(query)
        
      }
    })
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
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      temp <- base::lengths(base::mget(base::ls(env),
                                       envir = as.environment(env))) 
      
      # ensure equal lengths
      if ((sum(temp)/temp[[1]]) != length(temp)) {
        stop(glue::glue("Error in IDBacApp::insertIntoIndividualSpectra(): IDBacApp::processXMLIndSpectra() provided variables of differing lengths: \n ",  
                        paste0(names(temp),"=",temp, collapse = ", ")))
      } else { 
        query <- DBI::dbSendStatement(conn, 
                                      "INSERT INTO 'individual_spectra'(
                                  'spectrum_mass_hash',
                                  'spectrum_intensity_hash',
                                  'xml_hash',
                                  'strain_id',
                                  'peak_matrix',
                                  'spectrum_intensity',
                                  'max_mass',
                                  'min_mass',
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
                                  VALUES ($spectrum_mass_hash,
                                  $spectrum_intensity_hash,
                                  $xml_hash,
                                  $strain_id,
                                  $peak_matrix,
                                  $spectrum_intensity,
                                  $max_mass,
                                  $min_mass,
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
        
        
        if (is.null(acquisitionInfo) || length(acquisitionInfo) == 0L ) {
          acquisitionInfo <- rbind(rep(NA, temp[[1]]))       
        } 
        
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
        
        
        
        
        
        DBI::dbBind(query, 
                    c(
                      list(spectrum_mass_hash = env$spectrum_mass_hash,
                           spectrum_intensity_hash = env$spectrum_intensity_hash,
                           xml_hash = mzMLHash,
                           strain_id = sampleID,
                           peak_matrix = env$peak_matrix,
                           spectrum_intensity = env$spectrum_intensity,
                           min_mass = env$min_mass,
                           max_mass = env$max_mass,
                           ignore = ignore
                      ),
                      acquisitionInfo
                      
                      
                    ))
        
        
        
        DBI::dbClearResult(query)
        
      }
    })
}


