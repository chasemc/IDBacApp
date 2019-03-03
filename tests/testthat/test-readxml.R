context("test-readxml")

mz <- tempfile(fileext = ".mzML")

s <- MALDIquant::createMassSpectrum(mass = 1:100,
                                    intensity = rnorm(100)^2,
                                    metaData = list(name = "example spectrum"))

MALDIquantForeign::exportMzMl(s, mz)

a <- IDBacApp::readXML(mz)




test_that("multiplication works", {
  expect_identical(class(a)[[1]], "xml_document")
  # Can't gaurantee a length b/c file-path is included within XML
  expect_gt(nchar(as.character(a)), 4000)
})
