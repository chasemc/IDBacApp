context("test-binnr")

set.seed(42)

p <- c(MALDIquant::createMassPeaks(mass = c(2000, 3000, 4000),
                                   intensity = c(1, 1, 1)),
       MALDIquant::createMassPeaks(mass = c(2000, 3001, 4005),
                                   intensity = rep(1, 2, 3)),
       MALDIquant::createMassPeaks(mass = c(2000.01,3002,10000),
                                   intensity = rep(1, 2, 3)))


binned <- IDBacApp::createFuzzyVector(
                               ppm = 350,
                               massStart = 1000,
                               massEnd = 15000,
                               massList = lapply(p, function(x)x@mass),
                               intensityList = lapply(p, function(x)x@intensity))





test_that("fuzzification works", {
  expect_known_hash(binned, "24e3958c9d")
  expect_equal(dim(binned),
               c(42864, 3))
  expect_equal(class(binned)[[1]],
               "dgCMatrix")

})














