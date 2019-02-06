context("test-binnr")

set.seed(42)

p <- c(MALDIquant::createMassPeaks(mass = c(1, 1.01, 3), 
                                   intensity = c(2, 1, 1)),
       MALDIquant::createMassPeaks(mass = c(1.11, 3),
                                   intensity = rep(1, 2)),
       MALDIquant::createMassPeaks(mass = c(1.2, 3),
                                   intensity = rep(1, 2)))


binned <- IDBacApp::peakBinner(peakList = p,
                      ppm = 100000)






test_that("multiplication works", {
  expect_equal(digest::sha1(binned), "032cf3dc5d0cb6e9a13d84541f7096924fcb9432")
  expect_warning(IDBacApp::peakBinner(peakList = list("A"),
                                      ppm = 100000))
})














