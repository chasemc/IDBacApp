context("test-multidimensionalAnalysis")

set.seed(1)
mtcars <- as.matrix(mtcars)
a <- pcaCalculation(mtcars,
               logged = TRUE,
               scaled = TRUE,
               centered = TRUE,
               missing = .00001)



test_that("binnR works", {
  expect_equal(digest::sha1(a), "3f3f1033c3ae91d4e42baf506398546fabdef4fd")
  
})


