context("test-readacqufile")


a <- system.file("extdata", file.path("RawData","0_C1","1","1SLin","fid")  , package = "IDBacApp")
a <- IDBacApp::readAcqusFile(a)
a <- a[names(a) != "file"]

test_that("Parse Bruker acqu", {
  expect_known_hash(a, "2a07967348")
})
