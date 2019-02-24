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
  
  
  sqlDataFrame$XML <- c("mzMLHash",
                        "XML", 
                        "manufacturer",
                        "model",
                        "ionisation",
                        "analyzer",
                        "detector",
                        "Instrument_MetaFile")
  
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$XML)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$XML
  sqlDataFrame$XML <- temp
  
  
  sqlDataFrame$IndividualSpectra <- c("spectrumMassHash",
                                      "spectrumIntensityHash",
                                      "mzMLHash",
                                      "Strain_ID",
                                      "MassError",
                                      "AcquisitionDate",
                                      "proteinPeaksIntensity",
                                      "proteinPeaksSNR",
                                      "proteinPeaksMass",
                                      "proteinSpectrumIntensity",
                                      "smallMoleculePeaksIntensity",
                                      "smallMoleculePeaksSNR",
                                      "smallMoleculePeaksMass",
                                      "smallMoleculeSpectrumIntensity",
                                      "ignore")
  
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$IndividualSpectra)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$IndividualSpectra
  sqlDataFrame$IndividualSpectra <- temp
  
  
  
  sqlDataFrame$IndividualSpectraSQL <-
    
    "CREATE TABLE IndividualSpectra (
  spectrumMassHash                     TEXT,
  spectrumIntensityHash                TEXT,
  mzMLHash                             TEXT,
  Strain_ID                            TEXT,
  MassError                            REAL,
  AcquisitionDate                      TEXT,
  proteinPeaksIntensity                BLOB,
  proteinPeaksMass                     BLOB,
  proteinPeaksSNR                      BLOB,
  proteinSpectrumIntensity             BLOB,
  smallMoleculePeaksIntensity          BLOB,
  smallMoleculePeaksSNR                BLOB,
  smallMoleculePeaksMass               BLOB,
  smallMoleculeSpectrumIntensity       BLOB,
  ignore                               INTEGER,
  
  UNIQUE(Strain_ID, spectrumMassHash, spectrumIntensityHash) ON CONFLICT IGNORE
  );"
  
  
  
  
  sqlDataFrame$massTable <- c("spectrumMassHash",
                              "binaryMassVector")
  
  temp <- as.data.frame(matrix(nrow = numberScans,
                               ncol = length(sqlDataFrame$massTable)))
  
  dimnames(temp)[[2]] <- sqlDataFrame$massTable
  sqlDataFrame$massTable <- temp
  
  
  sqlDataFrame$massTableSQL <-
    
    "CREATE TABLE massTable (
  spectrumMassHash    TEXT PRIMARY KEY,
  binaryMassVector    BLOB,
 
  UNIQUE(spectrumMassHash) ON CONFLICT IGNORE
  );"
  
  return(sqlDataFrame)
  
}
