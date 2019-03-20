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
  expect_known_hash(a1, "7c99636ec3")
  expect_known_hash(a2, "24df9e2d21")
  expect_known_hash(a3, "85590a6b3d")
  
    
  })
