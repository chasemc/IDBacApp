context("test-map384well")


a <- IDBacApp::map384Well()


test_that("384 well map hasn't changed", {

  expect_known_hash(a, "821bfaaf94")
  expect_identical(ncol(a), 24L)
  expect_identical(nrow(a), 16L)
  expect_identical(class(a), "matrix")
  })
