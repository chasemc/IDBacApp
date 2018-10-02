sqlTableArchitecture <- function(){


sqlDataFrame <- new.env(parent = parent.frame())

sqlDataFrame$Samples <- data.frame("Strain_ID"              = NA,
                             "Genbank_Accession"            = NA,
                             "NCBI_TaxID"                   = NA,
                             "Kingdom"                      = NA,
                             "Phylum"                       = NA,
                             "Class"                        = NA,
                             "Order"                        = NA,
                             "Family"                       = NA,
                             "Genus"                        = NA,
                             "Species"                      = NA,
                             "Strain"                       = NA,
                             "MALDI_Matrix"                 = NA,
                             "Agar_Media"                   = NA,
                             "Cultivation_Temp_Celsius"     = NA,
                             "Cultivation_Time_Days"        = NA,
                             "Cultivation_Other"            = NA,
                             "Agar_Media"                   = NA,
                             "User"                         = NA,
                             "PI_FirstName_LastName"        = NA,
                             "PI_ORCID"                     = NA,
                             "16S"                          = NA
                            )



sqlDataFrame$XML <- data.frame("SHA1" = NA,
                         "XML" = NA, # entire xml file as blob
                         "manufacturer"                 = NA,
                         "model"                        = NA,
                         "ionisation"                   = NA,
                         "analyzer"                     = NA,
                         "detector"                     = NA,
                         "Instrument_MetaFile" = NA

                        )






sqlDataFrame$IndividualSpectra <- data.frame("filesha1" = NA,
                                             "SHA1" = NA,
                                             "Strain_ID" = NA,
                                             "MassError" = NA,
                                             "AcquisitionDate" = NA,
                                             "proteinPeaks"              = NA,
                                             "proteinSpectrum"     = NA,
                                             "smallMoleculePeaks"        = NA,
                                             "smallMoleculeSpectrum" = NA,
                                             "proteinPeaksHash"          = NA,
                                             "proteinSpectrumHash" = NA,
                                             "smallMoleculePeaksHash"    = NA,
                                             "smallMoleculeSpectrumHash" = NA
)



sqlDataFrame

}
