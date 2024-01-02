#' SQL code to create the SQLite spectra table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#'
#'
sql_create_spectra_table <- function(sqlConnection) {
  if (!DBI::dbExistsTable(sqlConnection, "spectra")) {
    a <- DBI::dbSendStatement(
      sqlConnection,
      "CREATE TABLE `spectra` (
  spectrum_mass_hash                     TEXT,
  spectrum_intensity_hash                TEXT,
  xml_hash                              TEXT,
  strain_id                            TEXT,
  peak_matrix                           BLOB,
  spectrum_intensity                    BLOB,
  max_mass                              INTEGER,
  min_mass                              INTEGER,
  ignore                               INTEGER,
  number                               INTEGER,
  time_delay                            INTEGER,
  time_delta                            NUMERIC,
  calibration_constants                 TEXT,
  v1_tof_calibration                     TEXT,
  data_type                             TEXT,
  data_system                           TEXT,
  spectrometer_type                     TEXT,
  inlet                                TEXT,
  ionization_mode                       TEXT,
  acquisition_method                    TEXT,
  acquisition_date                      TEXT,
  acquisition_mode                      TEXT,
  tof_mode                              TEXT,
  acquisition_operator_mode              TEXT,
  laser_attenuation                     INTEGER,
  digitizer_type                        TEXT,
  flex_control_version                   TEXT,
  id                                   TEXT,
  instrument                           TEXT,
  instrument_id                         TEXT,
  instrument_type                       TEXT,
  mass_error                            NUMERIC,
  laser_shots                           INTEGER,
  patch                                TEXT,
  path                                 TEXT,
  laser_repetition                      TEXT,
  spot                                 TEXT,
  spectrum_type                         TEXT,
  target_count                          TEXT,
  target_id_string                       TEXT,
  target_serial_number                   TEXT,
  target_type_number                     TEXT,

  UNIQUE(strain_id, spectrum_mass_hash, spectrum_intensity_hash) ON CONFLICT IGNORE
  );"
    )


    DBI::dbClearResult(a)
  } else {
    warning("spectra table already exists")
  }
}





#' SQL code to create the SQLite mass_index table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#'
#'
sql_create_massindex_table <- function(sqlConnection) {
  if (!DBI::dbExistsTable(sqlConnection, "mass_index")) {
    a <- DBI::dbSendStatement(
      sqlConnection,
      "CREATE TABLE `mass_index` (
  spectrum_mass_hash    TEXT,
  mass_vector          BLOB,

  UNIQUE(spectrum_mass_hash) ON CONFLICT IGNORE
  );"
    )
    DBI::dbClearResult(a)
  } else {
    warning("mass_index table already exists")
  }
}




#' SQL code to create the SQLite metadata table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#'
#'
sql_create_metadata_table <- function(sqlConnection) {
  if (!DBI::dbExistsTable(sqlConnection, "metadata")) {
    a <- DBI::dbSendStatement(
      sqlConnection,
      "CREATE TABLE `metadata` (
'strain_id'                  TEXT,
'genbank_accession'          TEXT,
'ncbi_taxid'                 TEXT,
'kingdom'                    TEXT,
'phylum'                     TEXT,
'class'                      TEXT,
'order'                      TEXT,
'family'                     TEXT,
'genus'                      TEXT,
'species'                    TEXT,
'maldi_matrix'               TEXT,
'dsm_cultivation_media'      TEXT,
'cultivation_temp_celsius'   TEXT,
'cultivation_time_days'      TEXT,
'cultivation_other'          TEXT,
'user_firstname_lastname'    TEXT,
'user_orcid'                 TEXT,
'pi_firstname_lastname'      TEXT,
'pi_orcid'                   TEXT,
'dna_16s'                    TEXT,

  UNIQUE(strain_id) ON CONFLICT IGNORE
  );"
    )


    DBI::dbClearResult(a)
  } else {
    warning("metadata table already exists")
  }
}







#' SQL code to create the SQLite xml table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#'
#'
sql_create_xml_table <- function(sqlConnection) {
  if (!DBI::dbExistsTable(sqlConnection, "xml")) {
    a <- DBI::dbSendStatement(
      sqlConnection,
      "CREATE TABLE `xml` (
  xml_hash         TEXT,
  xml             BLOB,
  manufacturer    TEXT,
  model           TEXT,
  ionization      TEXT,
  analyzer        TEXT,
  detector        TEXT,
  instrument_metafile BLOB,

  UNIQUE(xml_hash) ON CONFLICT IGNORE
    );"
    )


    DBI::dbClearResult(a)
  } else {
    warning("xml table already exists")
  }
}






#' SQL code to create the SQLite version table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#'
#'
sql_create_version_table <- function(sqlConnection) {
  if (!DBI::dbExistsTable(sqlConnection, "version")) {
    a <- DBI::dbSendStatement(
      sqlConnection,
      "CREATE TABLE version (
                              idbac_version     TEXT,
                              r_version         TEXT,
                              db_version        TEXT
                              );"
    )


    DBI::dbClearResult(a)
  } else {
    warning("version table already exists")
  }
}



#' SQL code to create the SQLite locale table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#'
#'
sql_create_locale_table <- function(sqlConnection) {
  if (!DBI::dbExistsTable(sqlConnection, "locale")) {
    a <- DBI::dbSendStatement(
      sqlConnection,
      "CREATE TABLE locale (
  locale         TEXT
    );"
    )


    DBI::dbClearResult(a)
  } else {
    warning("locale table already exists")
  }
}
