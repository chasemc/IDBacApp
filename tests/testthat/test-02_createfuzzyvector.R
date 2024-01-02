context("test-createFuzzyVector")
set.seed(42)

binned <- createFuzzyVector(
  ppm = 1000,
  massStart = 3000,
  massEnd = 15000,
  massList = list(
    seq(3000, 15000, 10),
    3000
  ),
  intensityList = list(
    seq(3000, 15000, 10),
    3000
  )
)

test_that("createFuzzyVector() works", {
  expect_equal(
    dim(binned),
    c(4011, 2)
  )
  expect_true(inherits(
    binned,
    c("dgCMatrix")
  ))
  expect_true(inherits(
    binned,
    c("Matrix")
  ))
  expect_known_hash(as.integer(binned[, 1]), "9e7a4284ad")
  expect_known_hash(as.integer(binned[, 2]), "c1f1b863bf")
})
