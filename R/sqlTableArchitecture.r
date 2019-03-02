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
  
  sqlDataFrame$metaData <- c("Strain_ID",
                             "Genbank_Accession",
                             "NCBI_TaxID",
                             "Kingdom",
                             "Phylum",
                             "Class",
                             "Order",
                             "Family",
                             "Genus",
                             "Species",
                             "MALDI_Matrix",
                             "DSM_Agar_Media",
                             "Cultivation_Temp_Celsius",
                             "Cultivation_Time_Days",
                             "Cultivation_Other",
                             "User",
                             "User_ORCID",
                             "PI_FirstName_LastName",
                             "PI_ORCID",
                             "dna_16S")
  
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$metaData)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$metaData
  sqlDataFrame$metaData <- temp
  
  
  sqlDataFrame$XML <- c("XMLHash",
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
                                      "XMLHash",
                                      "Strain_ID",
                                      "MassError",
                                      "AcquisitionDate",
                                      "peakMatrix",
                                      "spectrumIntensity",
                                      "maxMass",
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
  if (!DBI::dbExistsTable(sqlConnection, "massTable")) {
    
    a <- DBI::dbSendStatement(sqlConnection, 
                              "CREATE TABLE `IndividualSpectra` (
  spectrumMassHash                     TEXT,
  spectrumIntensityHash                TEXT,
  XMLHash                              TEXT,
  Strain_ID                            TEXT,
  MassError                            REAL,
  AcquisitionDate                      TEXT,
  peakMatrix                           TEXT,
  spectrumIntensity                    BLOB,
  maxMass                              INTEGER,
  ignore                               INTEGER,
  
  UNIQUE(Strain_ID, spectrumMassHash, spectrumIntensityHash) ON CONFLICT IGNORE
  );"
    )
    
    
    DBI::dbClearResult(a)
  } else {
    warning("IndividualSpecctra table already exists")
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
'Strain_ID'                  TEXT,     
'Genbank_Accession'          TEXT,             
'NCBI_TaxID'                 TEXT,       
'Kingdom'                    TEXT,   
'Phylum'                     TEXT,   
'Class'                      TEXT, 
'Order'                      TEXT, 
'Family'                     TEXT,   
'Genus'                      TEXT, 
'Species'                    TEXT,   
'MALDI_Matrix'               TEXT,         
'DSM_Agar_Media'             TEXT,           
'Cultivation_Temp_Celsius'   TEXT,                     
'Cultivation_Time_Days'      TEXT,                 
'Cultivation_Other'          TEXT,             
'User'                       TEXT, 
'User_ORCID'                 TEXT,       
'PI_FirstName_LastName'      TEXT,                 
'PI_ORCID'                   TEXT,     
'dna_16S'                    TEXT,   
 
  UNIQUE(Strain_ID) ON CONFLICT IGNORE
  );"
    )
    
    
    DBI::dbClearResult(a)
  } else {
    warning("metaData table already exists")
  }
  
}







#' SQL code to create the SQLite xxml table
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
  XMLHash         TEXT,
  XML             BLOB,
  manufacturer    TEXT,
  model           TEXT,
  ionization      TEXT,
  analyzer        TEXT,
  detector        TEXT,
  Instrument_MetaFile BLOB,

  UNIQUE(XMLHash) ON CONFLICT IGNORE
    );"
    )
    
    
    DBI::dbClearResult(a)
  } else {
    warning("metaData table already exists")
  }
}
