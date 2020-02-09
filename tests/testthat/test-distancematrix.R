context("test-distancematrix")

set.seed(42)

a1 <- distMatrix(data = mtcars,
                          method = "cosine")
a2 <- distMatrix(data = mtcars,
                          method = "euclidean")
a3 <- distMatrix(data = mtcars,
                          method = "manhattan")


test_that("distanceMatrix works", {
  expect_known_hash(a1, "7c99636ec3")
  expect_known_hash(a2, "37b6e4eddd")
  expect_known_hash(a3, "4dec2bb575")
 expect_error(distMatrix(data = mtcars[,1],
                       method = "manhattan"))

  })
