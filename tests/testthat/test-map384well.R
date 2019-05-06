context("test-map384well")


a <- IDBacApp::map384Well()


test_that("map384Well() hasn't changed", {

  expect_known_hash(a, "9b3fa92fa4")
  expect_identical(ncol(a), 24L)
  expect_identical(nrow(a), 16L)
  expect_identical(class(a), "matrix")
  })




a <- IDBacApp::nulledMap384Well()

test_that("nulledMap384Well() hasn't changed", {
  
  expect_known_hash(a, "243dc4f163")
  expect_identical(ncol(a), 24L)
  expect_identical(nrow(a), 16L)
  expect_identical(class(a), "data.frame")
  expect_identical(length(is.na(a)), 384L)
})
