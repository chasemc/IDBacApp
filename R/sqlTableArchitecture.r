#' sqlTableArchitecture
#'
#' @param numberScans NA 
#'
#' @return NA
#' @export
#'
sqlTableArchitecture <- function(numberScans){
  
  sqlDataFrame <- new.env(parent = parent.frame())
  
  sqlDataFrame$version <- data.frame(version = "1")
  
  sqlDataFrame$metaData <- c("strain_id",
                             "genbank_accession",
                             "ncbi_taxid",
                             "kingdom",
                             "phylum",
                             "class",
                             "order",
                             "family",
                             "genus",
                             "species",
                             "maldi_matrix",
                             "dsm_cultivation_media",
                             "cultivation_temp_celsius",
                             "cultivation_time_days",
                             "cultivation_other",
                             "user_firstname_lastname",
                             "user_orcid",
                             "pi_firstname_lastname",
                             "pi_orcid",
                             "dna_16s")
  
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$metaData)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$metaData
  sqlDataFrame$metaData <- temp
  
  
  sqlDataFrame$XML <- c("xml_hash",
                        "XML", 
                        "manufacturer",
                        "model",
                        "ionization",
                        "analyzer",
                        "detector",
                        "Instrument_MetaFile")
  
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$XML)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$XML
  sqlDataFrame$XML <- temp
  
  
  sqlDataFrame$IndividualSpectra <- c("spectrumMassHash",
                                      "spectrumIntensityHash",
                                      "xml_hash",
                                      "strain_id",
                                      "MassError",
                                      "AcquisitionDate",
                                      "peakMatrix",
                                      "spectrumIntensity",
                                      "maxMass",
                                      "minMass",
                                      "ignore")
  
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$IndividualSpectra)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$IndividualSpectra
  sqlDataFrame$IndividualSpectra <- temp
  
  
  
  sqlDataFrame$massTable <- c("spectrumMassHash",
                              "massVector")
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$massTable)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$massTable
  sqlDataFrame$massTable <- temp
  
  
  return(sqlDataFrame)
  
}





#' SQL code to create the SQLite IndividualSpectra table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#' @export
#'
sql_CreateIndividualSpectra <- function(sqlConnection){
  if (!DBI::dbExistsTable(sqlConnection, "IndividualSpectra")) {
    
    a <- DBI::dbSendStatement(sqlConnection, 
                              "CREATE TABLE `IndividualSpectra` (
  spectrumMassHash                     TEXT,
  spectrumIntensityHash                TEXT,
  xml_hash                              TEXT,
  strain_id                            TEXT,
  peakMatrix                           BLOB,
  spectrumIntensity                    BLOB,
  maxMass                              INTEGER,
  minMass                              INTEGER,
  ignore                               INTEGER,
  number                               INTEGER,
  timeDelay                            INTEGER,
  timeDelta                            NUMERIC,
  calibrationConstants                 TEXT,
  v1tofCalibration                     TEXT,
  dataType                             TEXT,
  dataSystem                           TEXT,
  spectrometerType                     TEXT,
  inlet                                TEXT,
  ionizationMode                       TEXT,
  acquisitionMethod                    TEXT,
  acquisitionDate                      TEXT,
  acquisitionMode                      TEXT,
  tofMode                              TEXT,
  acquisitionOperatorMode              TEXT,
  laserAttenuation                     INTEGER,
  digitizerType                        TEXT,
  flexControlVersion                   TEXT,
  id                                   TEXT,
  instrument                           TEXT,
  instrumentId                         TEXT,
  instrumentType                       TEXT,
  massError                            NUMERIC,
  laserShots                           INTEGER,
  patch                                TEXT,
  path                                 TEXT,
  laserRepetition                      TEXT,
  spot                                 TEXT,
  spectrumType                         TEXT,
  targetCount                          TEXT,
  targetIdString                       TEXT,
  targetSerialNumber                   TEXT,
  targetTypeNumber                     TEXT,
  
  UNIQUE(strain_id, spectrumMassHash, spectrumIntensityHash) ON CONFLICT IGNORE
  );"
    )
    
    
    DBI::dbClearResult(a)
  } else {
    warning("IndividualSpectra table already exists")
  }
}





#' SQL code to create the SQLite massTable table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#' @export
#'
sql_CreatemassTable <- function(sqlConnection){
  
  if (!DBI::dbExistsTable(sqlConnection, "massTable")) {
    
    a <- DBI::dbSendStatement(sqlConnection,
                              "CREATE TABLE `massTable` (
  spectrumMassHash    TEXT,
  massVector          BLOB,
 
  UNIQUE(spectrumMassHash) ON CONFLICT IGNORE
  );"
    )
    DBI::dbClearResult(a)
  } else {
    warning("massTable table already exists")
  }
}




#' SQL code to create the SQLite metaData table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#' @export
#'
sql_CreatemetaData <- function(sqlConnection){
  
  if (!DBI::dbExistsTable(sqlConnection, "metaData")) {
    
    a <- DBI::dbSendStatement(sqlConnection,
                              "CREATE TABLE `metaData` (
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
    warning("metaData table already exists")
  }
  
}







#' SQL code to create the SQLite xml table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#' @export
#'
sql_CreatexmlTable <- function(sqlConnection){
  
  if (!DBI::dbExistsTable(sqlConnection, "XML")) {
    
    a <- DBI::dbSendStatement(sqlConnection,
                              "CREATE TABLE `XML` (
  xml_hash         TEXT,
  XML             BLOB,
  manufacturer    TEXT,
  model           TEXT,
  ionization      TEXT,
  analyzer        TEXT,
  detector        TEXT,
  Instrument_MetaFile BLOB,

  UNIQUE(xml_hash) ON CONFLICT IGNORE
    );"
    )
    
    
    DBI::dbClearResult(a)
  } else {
    warning("XML table already exists")
  }
}






#' SQL code to create the SQLite version table
#'
#' @param sqlConnection sqlConnection
#'
#' @return SQL code as character
#' @export
#'
sql_CreateVersionTable <- function(sqlConnection){
  
  if (!DBI::dbExistsTable(sqlConnection, "version")) {
    
    a <- DBI::dbSendStatement(sqlConnection,
                              "CREATE TABLE version (
  version         INTEGER,
  UNIQUE(version) ON CONFLICT IGNORE
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
#' @export
#'
sql_CreatelLocaleTable <- function(sqlConnection){
  
  if (!DBI::dbExistsTable(sqlConnection, "locale")) {
    
    a <- DBI::dbSendStatement(sqlConnection,
                              "CREATE TABLE locale (
  locale         TEXT,
  UNIQUE(locale) ON CONFLICT IGNORE
    );"
    )
    
    
    DBI::dbClearResult(a)
  } else {
    warning("locale table already exists")
  }
}





