context("test-binnr")

set.seed(42)

p <- c(MALDIquant::createMassPeaks(mass = c(5000, 5008),
                                   intensity = c(5,5)),
       MALDIquant::createMassPeaks(mass = c(3000, 3001, 4005),
                                   intensity = rep(1, 2, 3)),
       MALDIquant::createMassPeaks(mass = c(3000,3002,10000),
                                   intensity = rep(1, 2, 3)))


binned <- IDBacApp::createFuzzyVector(ppm = 1000,
                                      massStart = 3000,
                                      massEnd = 15000,
                                      massList = lapply(p, function(x)x@mass),
                                      intensityList = lapply(p, function(x)x@intensity))

#plot(binned[,1], type = "l", xlim = c(660,690))
#points(binned[,1],xlim = c(660,690))



test_that("createFuzzyVector() works", {
  expect_equal(dim(binned),
               c(4011, 3))
  expect_true(inherits(binned,
                       c("dgCMatrix")))
  expect_true(inherits(binned,
                       c("Matrix")))
  expect_known_hash(binned, "a91a677653")
  
})














