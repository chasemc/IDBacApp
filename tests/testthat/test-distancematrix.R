context("test-distancematrix")

set.seed(42)

a1 <- IDBacApp::distMatrix(data = mtcars,
                          booled = T,
                          method = "cosine")
a2 <- IDBacApp::distMatrix(data = mtcars,
                          booled = F,
                          method = "euclidean")
a3 <- IDBacApp::distMatrix(data = mtcars,
                          booled = T,
                          method = "manhattan")


test_that("distanceMatrix works", {
  expect_known_hash(a1, "329b3bee83")
  expect_known_hash(a2, "37b6e4eddd")
  expect_known_hash(a3, "19170ed1d6")
 expect_error(IDBacApp::distMatrix(data = mtcars[,1],
                       booled = T,
                       method = "manhattan"))


  })
