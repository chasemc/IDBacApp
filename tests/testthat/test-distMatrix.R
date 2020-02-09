
a <- lapply(c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "cosine"),
       function(x) {
         
         a <- distMatrix(data = mtcars,
                                   method = x)
         round(a, 4)
       })



test_that("distMatrix hasn't changed", {
  expect_known_hash(a, "320fefcd92")
  
})
