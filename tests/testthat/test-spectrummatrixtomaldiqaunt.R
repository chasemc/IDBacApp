context("test-spectrummatrixtomaldiqaunt")

a <- cbind(1:10, 1:10)
b <- spectrumMatrixToMALDIqaunt(a)

test_that("spectrumMatrixToMALDIqaunt", {
  expect_identical(b[[1]]@mass, a[,1])
  expect_identical(b[[1]]@intensity, seq(10, 100, 10))
  expect_true(MALDIquant::isMassSpectrumList(b))
})
