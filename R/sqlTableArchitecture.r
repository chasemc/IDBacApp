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
  
  
  
  sqlDataFrame$XML <- c("mzMLSHA",
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
  
  
  
  sqlDataFrame$IndividualSpectra <- c("spectrumSHA",
                                      "mzMLSHA",
                                      "Strain_ID",
                                      "MassError",
                                      "AcquisitionDate",
                                      "proteinPeaks",
                                      "proteinSpectrum",
                                      "smallMoleculePeaks",
                                      "smallMoleculeSpectrum",
                                      "ignore")
  
  sqlDataFrame$IndividualSpectra <- as.data.frame(matrix(nrow = 1,
                                                         ncol = length(sqlDataFrame$IndividualSpectra), 
                                                         dimnames = list(NULL,sqlDataFrame$IndividualSpectra)))
  
  
  sqlDataFrame
  
}
