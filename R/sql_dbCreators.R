
# version table -----------------------------------------------------------



#' Create version table 
#'
#' @param userDBCon sqlite connection
#'
#' @return NA
#' 
#'
sql_fill_version_table <- function(userDBCon) {
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      ver <- cbind.data.frame(idbac_version = as.character(packageVersion("IDBacApp")), 
                              r_version = as.character(serial(sessionInfo()$R.version)))
      # Add version table
      DBI::dbWriteTable(conn = conn,
                        name = "version", # SQLite table to insert into
                        ver, # Insert single row into DB
                        append = TRUE, # Append to existing table
                        overwrite = FALSE) # Do not overwrite
      
    })
}


# locale table ------------------------------------------------------------


#' Insert current locale info into sql table 
#'
#' @param userDBCon  database connection
#'
#' @return side effect
#' 
#'
sql_fill_locale_table <- function(userDBCon) {
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      locale <- Sys.getlocale(category = "LC_ALL")
      locale <- as.character(locale)[[1]]
      locale <- as.data.frame(locale)
      
      # Add version table
      DBI::dbWriteTable(conn = conn,
                        name = "locale", # SQLite table to insert into
                        locale, # Insert single row into DB
                        append = TRUE, # Append to existing table
                        overwrite = FALSE) # Do not overwrite
      
    })
}  





# metadata table ----------------------------------------------------------



#' createMetaSQL
#'
#' @param sampleID NA
#' @param userDBCon NA
#'
#' @return NA
#' 
#'

createMetaSQL <- function(sampleID,
                          userDBCon){
  
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      if (!DBI::dbExistsTable(conn, "metadata")) {
        sql_create_metadata_table(conn)
      }  
      
      query <- DBI::dbSendStatement(conn, 
                                    "INSERT INTO 'metadata' (
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
#' 
#'

createXMLSQL <- function(rawDataFilePath,
                         userDBCon,
                         mzML_con){
  
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      xmlFile <- serializeXML(rawDataFilePath)
      
      mzMLHash <- hashR(xmlFile)
      
      if (!DBI::dbExistsTable(conn, "xml")) {
        sql_create_xml_table(conn)
      }
      
      
      # Get instrument Info
      instInfo <- mzR::instrumentInfo(mzML_con)
      
      
      # # Find acquisitionInfo from mzML file
      #acquisitionInfo <- findAcquisitionInfo(rawDataFilePath,
      #                                               instInfo$manufacturer)
      
      # if ("instrument_metafile" %in% ls(acquisitionInfo)) { 
      #   sqlDataFrame$xml$instrument_metafile <- serial(acquisitionInfo$instrument_metafile)
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
#' @param userDBCon NA
#' @param sampleID NA
#' @param XMLinfo NA
#' @param smallRangeEnd end of mass region for small mol, if m/z above this- will be classified as "protein" spectrum
#' @param acquisitionInfo acquisitionInfo (currently only used when converting from Bruker raw data)
#'
#' @return NA
#' 
#'
createSpectraSQL <- function(mzML_con, 
                             userDBCon,
                             sampleID,
                             XMLinfo,
                             smallRangeEnd = 6000,
                             acquisitionInfo){
  
  
  
  spectraImport <- mzR::peaks(mzML_con)
  
  spectraImport <- spectrumMatrixToMALDIqaunt(spectraImport)
  
  
  # logical vector of maximum masses of mass vectors. True = small mol, False = protein
  smallIndex <- unlist(lapply(spectraImport, function(x) max(x@mass)))
  smallIndex <- smallIndex < smallRangeEnd
  
  
  # Small mol spectra -------------------------------------------------------
  
  if (any(smallIndex)) { 
    env <- processXMLIndSpectra(spectraImport = spectraImport,
                                smallOrProtein = "small",
                                index = smallIndex)
    
    insertIntoIndividualSpectra(env = env,
                                XMLinfo = XMLinfo,
                                userDBCon = userDBCon,
                                acquisitionInfo = acquisitionInfo[smallIndex],
                                sampleID = sampleID)
    insertIntoMassTable(env = env,
                        userDBCon = userDBCon)
  }
  # Protein Spectra ---------------------------------------------------------
  
  if (any(!smallIndex)) {
    
    env <- processXMLIndSpectra(spectraImport = spectraImport,
                                smallOrProtein = "protein",
                                index = !smallIndex)
    insertIntoIndividualSpectra(env = env,
                                XMLinfo = XMLinfo,
                                userDBCon = userDBCon,
                                acquisitionInfo = acquisitionInfo[!smallIndex],
                                sampleID = sampleID)
    insertIntoMassTable(env = env,
                        userDBCon = userDBCon)
  }
  
  
}






#' Write mass_index data to SQLite
#'
#' @param env environment 
#' @param userDBCon checked database connection
#'
#' @return nothing, writes to database
#' 
#'
insertIntoMassTable <- function(env,
                                userDBCon){
  
  pool::poolWithTransaction(
    pool = userDBCon,
    func = function(conn){
      
      if (length(env$spectrum_mass_hash) != length(env$mass_vector)) {
        stop("Error in insertIntoMassTable(): processXMLIndSpectra() provided
                    spectrum_mass_hash and mass_vector variables with different lengths")
      } else { 
        query <- DBI::dbSendStatement(conn, 
                                      "INSERT INTO 'mass_index'(
                              'spectrum_mass_hash',
                              'mass_vector')
                              VALUES (
                              $spectrum_mass_hash,
                              $mass_vector);")
        
        DBI::dbBind(query, list(spectrum_mass_hash = env$spectrum_mass_hash,
                                mass_vector = env$mass_vector)
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
#' 
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
        stop(glue::glue("Error in insertIntoIndividualSpectra(): processXMLIndSpectra() provided variables of differing lengths: \n ",  
                        paste0(names(temp),"=",temp, collapse = ", ")))
      } else { 
        query <- DBI::dbSendStatement(conn, 
                                      "INSERT INTO 'spectra'(
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
                                  'time_delay',
                                  'time_delta',
                                  'calibration_constants',
                                  'v1_tof_calibration',
                                  'data_type',
                                  'data_system',
                                  'spectrometer_type',
                                  'inlet',
                                  'ionization_mode',
                                  'acquisition_method',
                                  'acquisition_date',
                                  'acquisition_mode',
                                  'tof_mode',
                                  'acquisition_operator_mode',
                                  'laser_attenuation',
                                  'digitizer_type',
                                  'flex_control_version',
                                  'id',
                                  'instrument',
                                  'instrument_id',
                                  'instrument_type',
                                  'mass_error',
                                  'laser_shots',
                                  'patch',
                                  'path',
                                  'laser_repetition',
                                  'spot',
                                  'spectrum_type',
                                  'target_count',
                                  'target_id_string',
                                  'target_serial_number',
                                  'target_type_number')
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
                                  $time_delay,
                                  $time_delta,
                                  $calibration_constants,
                                  $v1_tof_calibration,
                                  $data_type,
                                  $data_system,
                                  $spectrometer_type,
                                  $inlet,
                                  $ionization_mode,
                                  $acquisition_method,
                                  $acquisition_date,
                                  $acquisition_mode,
                                  $tof_mode,
                                  $acquisition_operator_mode,
                                  $laser_attenuation,
                                  $digitizer_type,
                                  $flex_control_version,
                                  $id,
                                  $instrument,
                                  $instrument_id,
                                  $instrument_type,
                                  $mass_error,
                                  $laser_shots,
                                  $patch,
                                  $path,
                                  $laser_repetition,
                                  $spot,
                                  $spectrum_type,
                                  $target_count,
                                  $target_id_string,
                                  $target_serial_number,
                                  $target_type_number
                                  );"
        )
        
        
        
        
        ignore <- rep(0, times = temp[[1]])
        sampleID <- rep(sampleID[[1]], times = temp[[1]])
        mzMLHash <- rep(XMLinfo$mzMLHash, times = temp[[1]])
        
        
        
        a <- c('number',
               'time_delay',
               'time_delta',
               'calibration_constants',
               'v1_tof_calibration',
               'data_type',
               'data_system',
               'spectrometer_type',
               'inlet',
               'ionization_mode',
               'acquisition_method',
               'acquisition_date',
               'acquisition_mode',
               'tof_mode',
               'acquisition_operator_mode',
               'laser_attenuation',
               'digitizer_type',
               'flex_control_version',
               'id',
               'instrument',
               'instrument_id',
               'instrument_type',
               'mass_error',
               'laser_shots',
               'patch',
               'path',
               'laser_repetition',
               'spot',
               'spectrum_type',
               'target_count',
               'target_id_string',
               'target_serial_number',
               'target_type_number')
        
        
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


