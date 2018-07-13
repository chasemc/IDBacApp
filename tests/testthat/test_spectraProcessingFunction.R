context("test_spectraProcessingFunction")



extData <- system.file("extdata", package="IDBacApp")



# remove files if already tested
a <- lapply(c("172-1_ProteinPeaks.rds",
  "172-1_SmallMoleculePeaks.rds",
  "172-1_SummedProteinSpectra.rds",
  "172-1_SummedSmallMoleculeSpectra.rds"), function(x) file.path(extData, "Example-Data_Already_Converted/IDBac/Peak_Lists", x))

if(any(sapply(a, file.exists))){
z <- lapply(a, file.remove)
}

b <- file.path(extData, "Example-Data_Already_Converted/IDBac/Converted_To_mzXML")
b <- list.files(b, pattern = ".mz", full.names = TRUE,ignore.case = TRUE)
spectraProcessingFunction(rawDataFilePaths = b[[1]],
                          idbacDirectory = file.path(extData, "Example-Data_Already_Converted/IDBac"))



test_that("mzXML peak picking and processing works", {
  expect_true(file.exists(a[[1]]))
  expect_true(file.exists(a[[2]]))
  expect_true(file.exists(a[[3]]))
  expect_true(file.exists(a[[4]]))

})



