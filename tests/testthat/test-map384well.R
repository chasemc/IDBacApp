context("test-map384well")


a <- IDBacApp::map384Well()


test_that("384 well map hasn't changed", {

  expect_known_hash(a, "90b5e1724d")
  expect_identical(ncol(a), 24)
  expect_identical(nrow(a), 16)
  expect_identical(class(a), "matrix")
  })
