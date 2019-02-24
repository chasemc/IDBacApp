context("test-spectrummatrixtomaldiqaunt")

a <- cbind(1:10, 1:10)
b <- spectrumMatrixToMALDIqaunt(a)

test_that("multiplication works", {
  expect_identical(b@mass, a[,1])
  expect_identical(b@intesnity, a[,2])
  expect_true(isMassSpectrumList(b))
})
