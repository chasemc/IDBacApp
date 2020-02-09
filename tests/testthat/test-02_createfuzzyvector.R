context("test-createFuzzyVector")

set.seed(42)

p <- c(MALDIquant::createMassPeaks(mass = c(5000, 5008),
                                   intensity = c(5,5)),
       MALDIquant::createMassPeaks(mass = c(3000, 3001, 4005),
                                   intensity = rep(1, 2, 3)),
       MALDIquant::createMassPeaks(mass = c(3000,3002,10000),
                                   intensity = rep(1, 2, 3)))

binned <- createFuzzyVector(ppm = 1000,
                                      massStart = 3000,
                                      massEnd = 15000,
                                      massList = lapply(p, function(x)x@mass),
                                      intensityList = lapply(p, function(x)x@intensity))


test_that("createFuzzyVector() works", {
  expect_equal(dim(binned),
               c(4011, 3))
  expect_true(inherits(binned,
                       c("dgCMatrix")))
  expect_true(inherits(binned,
                       c("Matrix")))
  expect_known_hash(binned, "64394656b1")
  
})














