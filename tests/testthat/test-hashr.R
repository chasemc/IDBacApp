context("test-hashr")

set.seed(42)

test_that("multiplication works", {
  expect_identical(hashR("Hello"), "2147ec83cdbb6d2624e62e3d1717795f7119d034")
})
