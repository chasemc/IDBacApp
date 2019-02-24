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
  
  
  sqlDataFrame$metaData <- as.data.frame(matrix(nrow = numberScans,
                                                ncol = length(sqlDataFrame$metaData), 
                                                dimnames = list(NULL,sqlDataFrame$metaData)))
  
  
  
  sqlDataFrame$XML <- c("mzMLHash",
                        "XML", 
                        "manufacturer",
                        "model",
                        "ionisation",
                        "analyzer",
                        "detector",
                        "Instrument_MetaFile")
  
  
  sqlDataFrame$XML <- as.data.frame(matrix(nrow = numberScans,
                                           ncol = length(sqlDataFrame$XML), 
                                           dimnames = list(NULL,sqlDataFrame$XML)))
  
  
  
  sqlDataFrame$IndividualSpectra <- c("spectrumMassHash",
                                      "spectrumIntensityHash",
                                      "mzMLHash",
                                      "Strain_ID",
                                      "MassError",
                                      "AcquisitionDate",
                                      "proteinPeaksIntensity",
                                      "proteinPeaksSNR",
                                      "proteinSpectrumIntensity",
                                      "smallMoleculePeaksIntensity",
                                      "smallMoleculePeaksSNR",
                                      "smallMoleculeSpectrumIntensity",
                                      "ignore")
  
  
  sqlDataFrame$IndividualSpectra <- as.data.frame(matrix(nrow = numberScans,
                                                         ncol = length(sqlDataFrame$IndividualSpectra), 
                                                         dimnames = list(NULL, sqlDataFrame$IndividualSpectra)))
  
  
  sqlDataFrame$IndividualSpectraSQL <-
    
    "CREATE TABLE IndividualSpectra (
  spectrumMassHash                     TEXT,
  spectrumIntensityHash                TEXT,
  mzMLHash                             TEXT,
  Strain_ID                            TEXT,
  MassError                            REAL,
  AcquisitionDate                      TEXT,
  proteinPeaksIntensity                BLOB,
  proteinPeaksSNR                      BLOB,
  proteinSpectrumIntensity             BLOB,
  smallMoleculePeaksIntensity          BLOB,
  smallMoleculePeaksSNR                BLOB,
  smallMoleculeSpectrumIntensity       BLOB,
  ignore                               INTEGER,
  
  UNIQUE(Strain_ID, spectrumMassHash, spectrumIntensityHash) ON CONFLICT IGNORE
  );"

  
  
  
  sqlDataFrame$massTable <- c("spectrumMassHash",
                              "binaryMassVector")
  
  sqlDataFrame$massTable <- as.data.frame(matrix(nrow = numberScans,
                                                         ncol = length(sqlDataFrame$massTable), 
                                                         dimnames = list(NULL,sqlDataFrame$massTable)))
  
  sqlDataFrame$massTableSQL <-
    
    "CREATE TABLE massTable (
  spectrumMassHash    TEXT PRIMARY KEY,
  binaryMassVector    BLOB,
 
  UNIQUE(spectrumMassHash) ON CONFLICT IGNORE
  );"
  
 return(sqlDataFrame)
  
}
