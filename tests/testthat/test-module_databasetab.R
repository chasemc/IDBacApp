context("test-module_databasetab")
set.seed(42)
suppressWarnings(
  a <- databaseTabUIFunc(function(x) x)
)
test_that("multiplication works", {
  testthat::expect_equal(class(a), c("shiny.tag.list", "list"))
})
