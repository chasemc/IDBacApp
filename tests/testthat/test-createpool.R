context("test-createpool")

a <- IDBacApp::createPool("a", tempdir())

test_that("create pool works", {
  expect_equal(class(a[[1]])[[1]], "Pool")
  expect_equal(class(a), "list")
  expect_equal(length(a), 1)
})
