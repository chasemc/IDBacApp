context("trimProteinSpectra")




extData <- system.file("extdata", package="IDBacApp")




trimmed <- trimProteinSpectra(injectLibrary = NULL,
                              idsToInject,
                              addToLibraryDendroLabel,
                              spectraPath = file.path(extData, "Example-Data_Already_Converted/IDBac"),
                              lowerMassCutoff = 4000,
                              upperMassCutoff = 5000,
                              massTolerance = .002)








test_that("MSConvert commands are in correct order", {
  expect_true(any(unlist(lapply(trimmed, function(x)  max(x@mass))) > 500))
  expect_true(any(unlist(lapply(trimmed, function(x)  max(x@mass))) < 5000), info = "Make sure upper cutoff worked")
  expect_false(any(unlist(lapply(trimmed, function(x)  max(x@mass))) > 5000))
  expect_false(any(unlist(lapply(trimmed, function(x)  max(x@mass))) < 4000), info = "Make sure lower cutoff worked")

})
