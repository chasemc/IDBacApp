context("test-multidimensionalAnalysis")

set.seed(42)
mtcars <- as.matrix(mtcars)
a <- pcaCalculation(mtcars,
               logged = TRUE,
               scaled = TRUE,
               centered = TRUE,
               missing = .00001)



test_that("PCA works", {
  expect_equal(digest::sha1(a), "440f819a56afcab75770283299bdbb88bf1bcfd5")
  
})




a <- tsneCalculation(dataMatrix = mtcars,
                     perplexity = 5,
                     theta = 0.5,
                     iterations = 10)

test_that("tSNE works", {
  expect_equal(digest::sha1(a), "297a72652f5ad76e5df325dbe412f5871ff291fd")
  
})




a <- pcoaCalculation(distanceMatrix = dist(mtcars))

test_that("PCoA works", {
  expect_equal(digest::sha1(a), "a9893fefc0dafe45def2d0b34f9a418e45d008ee")
  
})