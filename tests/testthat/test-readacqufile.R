context("test-readacqufile")

set.seed(42)
a <- system.file("extdata", file.path("RawData","0_C1","1","1SLin","fid")  , package = "IDBacApp")
a <- IDBacApp::readAcquFile(a)
test_that("Parse Bruker acqu", {
  expect_known_hash(a, "347cb5d5c7")
})
