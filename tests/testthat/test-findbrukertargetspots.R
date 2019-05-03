context("test-findbrukertargetspots")

a <- IDBacApp::findBrukerTargetSpots(system.file("extdata/RawData", package = "IDBacApp"))

test_that("multiplication works", {

  expect_known_hash(a, "cfb0aa9d02")
  
})
