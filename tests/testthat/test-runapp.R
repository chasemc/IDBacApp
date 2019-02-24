context("test-runApp")

test_that("run_app() is correct class", {
  testthat::expect_identical(class(IDBacApp::run_app()), "shiny.appobj")
  })
