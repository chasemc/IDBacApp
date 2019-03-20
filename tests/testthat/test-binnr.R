context("test-binnr")

set.seed(42)

p <- c(MALDIquant::createMassPeaks(mass = c(2000, 3000, 4000), 
                                   intensity = c(1, 1, 1)),
       MALDIquant::createMassPeaks(mass = c(2000, 3001, 4005),
                                   intensity = rep(1, 2, 3)),
       MALDIquant::createMassPeaks(mass = c(2000.01,3002,10000),
                                   intensity = rep(1, 2, 3)))


binned <- IDBacApp::peakBinner(peakList = p,
                               ppm = 300,
                               massStart = 1000,
                               massEnd = 15000)






test_that("multiplication works", {
  expect_known_hash(binned, "f5d2c83a3f")
  expect_warning(IDBacApp::peakBinner(peakList = list("A"),
                                      ppm = 100000))
  expect_identical(suppressWarnings(IDBacApp::peakBinner(peakList = list("A"),
                                      ppm = 100000)),
                   list())
})














