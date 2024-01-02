context("test-samplemapviewer")
a <- sampleMapViewer(nulledMap384Well())

test_that("samplemapviewer hasn't changed", {
  expect_known_hash(a, "4d0d3b0aa6")
})
