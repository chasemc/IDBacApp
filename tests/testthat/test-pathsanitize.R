context("test-pathsanitize")

a <- IDBacApp::sanitize("73r78y23i . ?f/.4_____f04!!23{}:>\"_++_")

test_that("path_sanitize works", {
  testthat::expect_identical(a, "73r78y23i_f_4_f04!!23{}_++_")
})
