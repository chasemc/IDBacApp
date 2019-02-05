context("test-mquanttomassvec")


p <- c(MALDIquant::createMassPeaks(mass = c(1, 1.01, 3), 
                                   intensity = c(2, 1, 1)),
       MALDIquant::createMassPeaks(mass = c(0.99, 3),
                                   intensity = rep(1, 2)),
       MALDIquant::createMassPeaks(mass = c(1.02, 3),
                                   intensity = rep(1, 2)))


p <- IDBacApp::mQuantToMassVec(p)

test_that("Maldiquant Peak Objects to mass vectors", {
  expect_equal(length(p), 3)
  expect_equal(p[[1]], c(1.00, 1.01, 3.00))
  expect_equal(p[[2]], c(0.99, 3.00))
  expect_equal(p[[3]], c(1.02, 3.00))
})
