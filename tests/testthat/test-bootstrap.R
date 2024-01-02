context("test-bootstrap")
set.seed(42)
a <- function(x) (hclust(dist(x)))
a <- bootstrap(mtcars, a)
b <- c(
  1,
  0.879,
  0.991,
  0.911,
  0.552,
  0.396,
  0.743,
  0.491,
  0.373,
  0.716,
  0.641,
  0.994,
  0.654,
  0.576,
  0.423,
  0.255,
  0.435,
  0.732,
  0.668,
  0.283,
  0.446,
  0.165,
  0.165,
  0.523,
  0.386,
  0.165,
  0.546,
  0.636,
  0.294,
  0.294,
  1
)
test_that("bootstap calculation", {
  expect_equal(a, b)
})
