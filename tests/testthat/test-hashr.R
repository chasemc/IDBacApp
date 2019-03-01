context("test-hashr")

set.seed(42)

test_that("multiplication works", {
  expect_identical(hashR("Hello"), "5e0ed0f2695a805a")
})
