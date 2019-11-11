

one <- sqlTableArchitecture(1)

five <- sqlTableArchitecture(5)



one_nrow <- lapply(ls(one), function(x) nrow(one[[x]]))
names(one_nrow) <- ls(one)
five_nrow <- lapply(ls(five), function(x) nrow(five[[x]]))
names(five_nrow) <- ls(five)

test_that("sqlTableArchitecture creates correct number of rows", {
  expect_equal(one_nrow$IndividualSpectra,
               1L)
  expect_equal(one_nrow$massTable,
               1L)
  expect_equal(one_nrow$metaData,
               1L)
  expect_equal(one_nrow$version,
               1L)
  expect_equal(one_nrow$XML,
               1L)

  
  expect_equal(five_nrow$IndividualSpectra,
               5L)
  expect_equal(five_nrow$massTable,
               5L)
  expect_equal(five_nrow$metaData,
               5L)
  expect_equal(five_nrow$version,
               1L)
  expect_equal(five_nrow$XML,
               5L)
})


cols <- lapply(ls(five), function(x) colnames(five[[x]]))
names(cols) <- ls(five)



test_that("sqlTableArchitecture creates correct table columns", {
  
expect_equal(cols$IndividualSpectra, 
             c("spectrumMassHash",
               "spectrumIntensityHash",
               "XMLHash",
               "Strain_ID",
               "MassError",
               "AcquisitionDate",
               "peakMatrix",
               "spectrumIntensity",
               "maxMass",
               "minMass",
               "ignore"))

  
  expect_equal(cols$massTable, 
               c("spectrumMassHash",
                 "massVector"))

  
  expect_equal(cols$metaData, 
               c("Strain_ID",
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
                 "dna_16S"))
  
  
  expect_equal(cols$version, 
               c("version"))
  
  expect_equal(cols$XML, 
               c("XMLHash",
                 "XML",
                 "manufacturer",
                 "model",
                 "ionization",
                 "analyzer",
                 "detector",
                 "Instrument_MetaFile"))
})


