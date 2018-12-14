context("test-mzxml_machinedata_readhelpers")


extData <- system.file("extdata", package="IDBacApp")
extData <- file.path(extData, "mzml", "1.mzML")

findRawSHAandFile_test <- findRawSHAandFile(extData)

extData <- system.file("extdata", package="IDBacApp")
extData <- file.path("file:/",extData, "RawData/0_C1/1/1SLin")

findAcquisitionInfo_test <- findAcquisitionInfo(extData,
                                                findRawSHAandFile_test$manufacturer)


test_that("Reading Bruker Flex mzML file metadata", {
  expect_identical("405471c2e40caef728bf0773d6ce10ad5d1bbd29",
                   findRawSHAandFile_test$spectrumSHA)
  expect_identical("file://C:\\Users\\CMC\\Desktop\\Example-Data_Subset\\Raw_Data - Copy (2)\\Protein_Data\\0_C1\\1\\1SLin",
                   findRawSHAandFile_test$rawFilePaths)
  expect_identical("Bruker Daltonics flex series",
                   findRawSHAandFile_test$manufacturer)
  
  
  
  # expect_identical(findAcquisitionInfo_test$spectrumSHA,
  #                  "405471c2e40caef728bf0773d6ce10ad5d1bbd29")
  # expect_identical(findAcquisitionInfo_test$rawFilePaths,
  #                  "file://C:\\Users\\CMC\\Desktop\\Example-Data_Subset\\Raw_Data - Copy (2)\\Protein_Data\\0_C1\\1\\1SLin")
  expect_identical(digest::digest(findAcquisitionInfo_test$Instrument_MetaFile, "sha1"),
                   "958252b3bccdda00fa123bf42a7df82d9bf27bbf")
  expect_equal(findAcquisitionInfo_test$MassError,
                   52.27669, tolerance = .01)
  expect_identical(findAcquisitionInfo_test$AcquisitionDate,
                   "2016-11-02T13:04:52.440-05:0")
  
  
  
  
  
})
