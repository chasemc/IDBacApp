context("test-bootstrap")

set.seed(42)
a <- function(x) (hclust(dist(x)))
a <- IDBacApp::bootstrap(mtcars,a)

b <- c(1,
       0.882,
       0.992,
       0.912,
       0.56,
       0.387,
       0.757,
       0.484,
       0.367,
       0.732,
       0.624,
       0.997,
       0.633,
       0.572,
       0.414,
       0.267,
       0.42,
       0.745,
       0.644,
       0.288,
       0.43,
       0.186,
       0.186,
       0.521,
       0.378,
       0.186,
       0.537,
       0.618,
       0.303,
       0.303,
       1)
test_that("bootstap calculation", {
  expect_identical(a, b)
})
