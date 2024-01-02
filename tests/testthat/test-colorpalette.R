context("test-colorpalette")
set.seed(42)
test_that("multiplication works", {
  expect_equal(digest::sha1(colorBlindPalette()), "02f7230f0c101f17d47a1288ebb493ef2bc86d01")
})
