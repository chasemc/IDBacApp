context("test-samplemapviewer")

a <- IDBacApp::sampleMapViewer(IDBacApp::nulledMap384Well())


test_that("samplemapviewer hasn't changed", {
  expect_known_hash(a, "4d0d3b0aa6")
  
})
