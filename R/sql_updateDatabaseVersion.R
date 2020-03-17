# Need to update:
# massTable -> mass_index
# metaData -> metadata    # This is actually ok b/c SQL= not case dependent
# XML -> xml              # This is actually ok b/c SQL= not case dependent
# IndividualSpectra -> spectra
# version bump

# How to get fields to save as text
# new_f <- lapply(new_tables, function(x) DBI::dbListFields(new_pool, x))
# a <- lapply(new_f,function(x){
#   paste0(
#     "c(",
#     paste0(
#       sQuote(x, 
#              q = "ASCII"),
# 
#       collapse = ","),
#     ")")})
# 
# paste0("list(",paste0(a, collapse = ","), ")")




#' Update IDBac Database from version 1 to 2
#'
#' @param pool pool object
#' @param copy_overwrite should the db be copied or overwritten?
#' @param latest_db_version current database version
#' @return NA, side effect
#' @export
#'
idbac_update_db <- function(pool,
                            copy_overwrite = "copy",
                            latest_db_version = "2.0.0"){
  
  
  .checkPool(pool)
  
  copy_overwrite <- switch(as.character(copy_overwrite),
                           "copy" = TRUE,
                           "overwrite" = FALSE,
                           stop("expected 'copy' or 'overwrite'")
  )
  
  # Check if update is needed -----------------------------------------------
  
  current_db_version <- tryCatch(pool::poolWithTransaction(pool, 
                                                           function(con){
                                                             DBI::dbGetQuery(con, 
                                                                             "SELECT db_version FROM version")
                                                           }),
                                 error = function(x) FALSE) 
  
  current_db_version <- tail(current_db_version, 1)[[1]]
  
  # Check if database is earlier than 'db_version' was implemented
  if (isFALSE(current_db_version)) {
    # Wrapped in return() to break out of function
    message(basename(.db_path_from_pool(pool)), 
            " is less than version 2.0.0, updating to newest database version.")
    current_db_version <- "0"
  } else {
    # Exit function if version is up to date
    if (compareVersion(current_db_version, latest_db_version) > -1L) {
      return(message(basename(.db_path_from_pool(pool)), 
                     " is IDBac database version: ",
                     current_db_version,
                     "\n",
                     "skipping update"))
    }
  }
  
  # Switch pool if in "copy" mode -------------------------------------------
  
  if (copy_overwrite) {
    new_pool_path <- .copy_db(pool = pool,
                              latest_db_version = latest_db_version)
    
    suppressWarnings(pool::poolClose(pool))
    
    pool <- idbac_connect(fileName = tools::file_path_sans_ext(basename(new_pool_path)),
                          filePath = dirname(new_pool_path))[[1]]
    .checkPool(pool)
  }
  
  
  if (compareVersion(current_db_version, "2.0.0") == -1L) {
    
    old_tables <- c("IndividualSpectra",
                    "XML",
                    "locale",
                    "massTable",
                    "metaData",
                    "version")
    
    new_tables <- c("spectra",
                    "xml",
                    "locale",
                    "mass_index",
                    "metadata",
                    "version")     
    
    old_fields <- "list(c('spectrumMassHash','spectrumIntensityHash','XMLHash','Strain_ID','peakMatrix','spectrumIntensity','maxMass','minMass','ignore','number','timeDelay','timeDelta','calibrationConstants','v1tofCalibration','dataType','dataSystem','spectrometerType','inlet','ionizationMode','acquisitionMethod','acquisitionDate','acquisitionMode','tofMode','acquisitionOperatorMode','laserAttenuation','digitizerType','flexControlVersion','id','instrument','instrumentId','instrumentType','massError','laserShots','patch','path','laserRepetition','spot','spectrumType','targetCount','targetIdString','targetSerialNumber','targetTypeNumber'),c('XMLHash','XML','manufacturer','model','ionization','analyzer','detector','Instrument_MetaFile'),c('locale'),c('spectrumMassHash','massVector'),c('Strain_ID','Genbank_Accession','NCBI_TaxID','Kingdom','Phylum','Class','Order','Family','Genus','Species','MALDI_Matrix','DSM_Agar_Media','Cultivation_Temp_Celsius','Cultivation_Time_Days','Cultivation_Other','User','User_ORCID','PI_FirstName_LastName','PI_ORCID','dna_16S'),c('IDBacVersion','rVersion'))"
    old_fields <- eval(str2lang(old_fields))
    
    
    new_fields <- "list(c('spectrum_mass_hash','spectrum_intensity_hash','xml_hash','strain_id','peak_matrix','spectrum_intensity','max_mass','min_mass','ignore','number','time_delay','time_delta','calibration_constants','v1_tof_calibration','data_type','data_system','spectrometer_type','inlet','ionization_mode','acquisition_method','acquisition_date','acquisition_mode','tof_mode','acquisition_operator_mode','laser_attenuation','digitizer_type','flex_control_version','id','instrument','instrument_id','instrument_type','mass_error','laser_shots','patch','path','laser_repetition','spot','spectrum_type','target_count','target_id_string','target_serial_number','target_type_number'),c('xml_hash','xml','manufacturer','model','ionization','analyzer','detector','instrument_metafile'),c('locale'),c('spectrum_mass_hash','mass_vector'),c('strain_id','genbank_accession','ncbi_taxid','kingdom','phylum','class','order','family','genus','species','maldi_matrix','dsm_cultivation_media','cultivation_temp_celsius','cultivation_time_days','cultivation_other','user_firstname_lastname','user_orcid','pi_firstname_lastname','pi_orcid','dna_16s'),c('idbac_version','r_version'))"
    new_fields <- eval(str2lang(new_fields))
    
    
    
    a <- mapply(
      function(old, new, table){
        
        glue::glue('ALTER TABLE {table} RENAME COLUMN "{old}" TO "{new}"')
        
      },
      old_fields,
      new_fields,
      old_tables)
    
    con <- pool::poolCheckout(pool)
    a <- unlist(a)
    
    for (i in a) {
      DBI::dbExecute(con, 
                     i)
    }
    
    DBI::dbExecute(con, 
                   'ALTER TABLE "IndividualSpectra"
                    RENAME TO "spectra";')
    DBI::dbExecute(con, 
                   'ALTER TABLE "massTable"
                    RENAME TO "mass_index";')
    
    pool::poolReturn(con)
    
    
    message(paste0(unlist(old_fields), " is now ", unlist(new_fields), collapse = "\n"))
    
    # Add db_version field if needed 
    delete_me <- tryCatch(DBI::dbGetQuery(pool, "SELECT db_version FROM version;"),
                          error = function(x){
                            pool::poolWithTransaction(pool, 
                                                      function(con){
                                                        
                                                        DBI::dbSendStatement(con, 
                                                                             "ALTER TABLE version
                                                                            ADD db_version TEXT;")
                                                      })
                          }) 
    remove(delete_me)
    
    
    sql_fill_locale_table(pool = pool)
    sql_fill_version_table(pool = pool)
    
    
    
    message(paste0("Updated database... \n",
                   "Installed IDBac version: ",  as.character(utils::packageVersion("IDBacApp")), "\n", 
                   "Database version: ", as.character(utils::packageVersion("IDBacApp"))))      
  }
  
}



#' Copy an IDBac database, appending db version number
#'
#' @param pool IDBac pool
#' @param latest_db_version current database version
#'
#' @return The path for the new database
.copy_db <- function(pool,
                     latest_db_version){
  
  .checkPool(pool)
  
  provided_db_path <- .db_path_from_pool(pool)
  
  new_name <- basename(provided_db_path)
  new_name <- tools::file_path_sans_ext(new_name)
  new_path <- file.path(dirname(provided_db_path),
                        paste0(new_name, 
                               "_db-",
                               sanitize(latest_db_version),
                               ".sqlite"))
  
  if (file.exists(new_path)) {
    stop("\n",
         new_path, 
         "\n",
         "alread exists, please delete it if you want to run IDBac update.\n",
         "Deleting may require manually closing the pool connection or restarting R.")
  }
  
  if (isFALSE(file.exists(provided_db_path))) {
    stop("Couldn't find database file to copy.")
  }
  
  message("Copying...",
          "\n",
          paste0(list(from = provided_db_path,
                      to = new_path), collapse = "\nto\n"))
  
  file.copy(from = provided_db_path,
            to = new_path,
            copy.mode = F
  )
  
  message("Finished copying")
  return(new_path)
}