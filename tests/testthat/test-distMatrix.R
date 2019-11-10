
a <- lapply(c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "cosine"),
       function(x) {
         
         a <- IDBacApp::distMatrix(data = mtcars,
                                   method = x,
                                   booled = T)
         round(a, 4)
       })



b <- lapply(c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski", "cosine"),
            function(x) {
              
              a <- IDBacApp::distMatrix(data = mtcars,
                                        method = x,
                                        booled = F)
              round(a, 4)
            })



test_that("distMatrix hasn't changed", {
  expect_known_hash(a, "0348388d46")
  expect_known_hash(b, "320fefcd92")
  
})
