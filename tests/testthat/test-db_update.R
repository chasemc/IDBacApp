# options(local_testing = TRUE)
# options(local_test_files = "C:/Users/chase/Documents/idbac_test_data")

if (isTRUE(.Options$local_testing)) {
  # Test with copy ----------------------------------------------------------
  top_dir <- .Options$local_test_files
  sql_dir <- file.path(
    top_dir,
    "sqlite_version_1-1-10"
  )
  temp_dir <- file.path(
    tempdir(),
    "tempsql"
  )
  dir.create(temp_dir)
  file.copy(
    file.path(
      sql_dir,
      "sqlite_version_1-1-10.sqlite"
    ),
    file.path(
      temp_dir,
      "sqlite_version_1-1-10.sqlite"
    )
  )
  pool <- IDBacApp::idbac_connect(
    "sqlite_version_1-1-10",
    temp_dir
  )[[1]]

  old_tables <- DBI::dbListTables(pool)
  test_that("idbac_update_db old database is indeed old", {
    expect_equal(
      old_tables,
      c("IndividualSpectra", "XML", "locale", "massTable", "metaData", "version")
    )
  })
  suppressWarnings(
    idbac_update_db(pool,
      copy_overwrite = "copy",
      latest_db_version = "2.0.0"
    )
  )
  pool <- IDBacApp::idbac_connect(
    "sqlite_version_1-1-10_db-2_0_0",
    temp_dir
  )[[1]]
  tables <- DBI::dbListTables(pool)
  fields <- lapply(tables, function(x) DBI::dbListFields(pool, x))
  labels(fields) <- tables
  expected_fields <- "list(c('spectrum_mass_hash','spectrum_intensity_hash','xml_hash','strain_id','peak_matrix','spectrum_intensity','max_mass','min_mass','ignore','number','time_delay','time_delta','calibration_constants','v1_tof_calibration','data_type','data_system','spectrometer_type','inlet','ionization_mode','acquisition_method','acquisition_date','acquisition_mode','tof_mode','acquisition_operator_mode','laser_attenuation','digitizer_type','flex_control_version','id','instrument','instrument_id','instrument_type','mass_error','laser_shots','patch','path','laser_repetition','spot','spectrum_type','target_count','target_id_string','target_serial_number','target_type_number'),c('xml_hash','xml','manufacturer','model','ionization','analyzer','detector','instrument_metafile'),c('locale'),c('spectrum_mass_hash','mass_vector'),c('strain_id','genbank_accession','ncbi_taxid','kingdom','phylum','class','order','family','genus','species','maldi_matrix','dsm_cultivation_media','cultivation_temp_celsius','cultivation_time_days','cultivation_other','user_firstname_lastname','user_orcid','pi_firstname_lastname','pi_orcid','dna_16s'),c('idbac_version','r_version', 'db_version'))"
  expected_fields <- eval(str2lang(expected_fields))
  labels(expected_fields)[[1]] <- "spectra"
  labels(expected_fields)[[2]] <- "XML"
  labels(expected_fields)[[3]] <- "locale"
  labels(expected_fields)[[4]] <- "mass_index"
  labels(expected_fields)[[5]] <- "metaData"
  labels(expected_fields)[[6]] <- "version"
  expected_fields <- expected_fields[match(labels(fields), labels(expected_fields))]

  test_that("idbac_update_db works", {
    expect_equal(
      tables,
      c("XML", "locale", "mass_index", "metaData", "spectra", "version")
    )
    expect_identical(fields, expected_fields)
  })

  # Test overwite db --------------------------------------------------------
  pool <- IDBacApp::idbac_connect(
    "sqlite_version_1-1-10",
    temp_dir
  )[[1]]
  suppressWarnings(
    idbac_update_db(pool,
      copy_overwrite = "overwrite",
      latest_db_version = "2.0.0"
    )
  )

  tables <- DBI::dbListTables(pool)
  fields <- lapply(tables, function(x) DBI::dbListFields(pool, x))
  labels(fields) <- tables
  expected_fields <- "list(c('spectrum_mass_hash','spectrum_intensity_hash','xml_hash','strain_id','peak_matrix','spectrum_intensity','max_mass','min_mass','ignore','number','time_delay','time_delta','calibration_constants','v1_tof_calibration','data_type','data_system','spectrometer_type','inlet','ionization_mode','acquisition_method','acquisition_date','acquisition_mode','tof_mode','acquisition_operator_mode','laser_attenuation','digitizer_type','flex_control_version','id','instrument','instrument_id','instrument_type','mass_error','laser_shots','patch','path','laser_repetition','spot','spectrum_type','target_count','target_id_string','target_serial_number','target_type_number'),c('xml_hash','xml','manufacturer','model','ionization','analyzer','detector','instrument_metafile'),c('locale'),c('spectrum_mass_hash','mass_vector'),c('strain_id','genbank_accession','ncbi_taxid','kingdom','phylum','class','order','family','genus','species','maldi_matrix','dsm_cultivation_media','cultivation_temp_celsius','cultivation_time_days','cultivation_other','user_firstname_lastname','user_orcid','pi_firstname_lastname','pi_orcid','dna_16s'),c('idbac_version','r_version', 'db_version'))"
  expected_fields <- eval(str2lang(expected_fields))
  labels(expected_fields)[[1]] <- "spectra"
  labels(expected_fields)[[2]] <- "XML"
  labels(expected_fields)[[3]] <- "locale"
  labels(expected_fields)[[4]] <- "mass_index"
  labels(expected_fields)[[5]] <- "metaData"
  labels(expected_fields)[[6]] <- "version"
  expected_fields <- expected_fields[match(labels(fields), labels(expected_fields))]

  test_that("idbac_update_db works", {
    expect_equal(
      tables,
      c("XML", "locale", "mass_index", "metaData", "spectra", "version")
    )
    expect_identical(fields, expected_fields)
  })
}
