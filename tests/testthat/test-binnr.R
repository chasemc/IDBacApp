# context("test-binnr")
# 
# 
# binned <- binnR(vectorlist = list(c(1:10)),
#                 ppm = 1,
#                 low = 1, 
#                 high = 15,
#                 increment = 0.5)
# result <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# 
# test_that("binnR works", {
#   expect_equal(length(binned), 1)
#   expect_equal(length(binned[[1]]), 29)
#   expect_equal(binned[[1]], result)
#   
#   
# })
# 
# 
