context("test-module_tsne")
set.seed(42)

a <- tsneUiPop(function(x)x, 
                         plotTitle = "tsne")

test_that("multiplication works", {
  testthat::expect_equal(class(a), "shiny.tag")
  #testthat::expect_known_hash(a, "c9379a4531")
})
