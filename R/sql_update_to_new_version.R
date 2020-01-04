

#' Get the IDBac version that created the database
#'
#' @param pool pool object
#'
#' @return character version if present, or NULL if error
#' @export
#'
get_db_version <- function(pool){
 
  res <- pool::dbGetQuery(pool,
                             'Select idbac_version
                 FROM version')[[1]]
  
  tryCatch(pool::dbGetQuery(pool,
                            'Select idbac_version
                            FROM version')[[1]],
           error = function(x) NULL)
  }

#' Update IDBac Database from version 1 to 2
#'
#' @param pool pool object
#'
#' @return NA, side effect
#' @export
#'
update_database_version <- function(pool){
  con <- pool::poolCheckout(pool)
  # Need to update:
  # massTable -> mass_index
  # metaData -> metadata    # This is actually ok b/c SQL= not case dependent
  # XML -> xml              # This is actually ok b/c SQL= not case dependent
  # IndividualSpectra -> spectra
  # version bump
  
  # How to get fields to save as text
  # new_f <- lapply(new_t, function(x) DBI::dbListFields(new_pool, x))
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
  
  
  
  # Make sure tables are in correct order
  old_t <- c("IndividualSpectra",
             "XML",
             "locale",
             "massTable",
             "metaData",
             "version")
  
  new_t <- c( "spectra",
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
    old_t)
  
  a <- unlist(a)
  
  
  
  for (i in a) {
    DBI::dbExecute(con, 
                   i)
  }
  
  # Change Table IDs --------------------------------------------------------
  
  # Note: xml and metadata tables don't have to
  # be modified because SQL is case insensitive
  
  DBI::dbExecute(con, 
                 'ALTER TABLE "IndividualSpectra"
                RENAME TO "spectra";')
  DBI::dbExecute(con, 
                 'ALTER TABLE "massTable"
                RENAME TO "mass_index";')
  
  
  pool::poolReturn(con)
  
}
