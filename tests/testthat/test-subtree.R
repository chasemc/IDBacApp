a <- tempdir()
sql_path <- file.path(a,
  "idbac_tests",
  "sqlite",
  fsep = "/"
)
sql_path <- normalizePath(
  sql_path,
  "/"
)
pool <- idbac_connect(
  "testdb",
  sql_path
)
dend1 <- as.dendrogram(hclust(dist(mtcars)))

subtrees <- get_subtrees(
  dend = dend1,
  h = 200
)
num_labs <- sapply(subtrees, function(x) length(labels(x)))
test_that("get_subtrees works, h=200", {
  expect_equal(
    num_labs,
    c(16, 7, 8, 1)
  )
})
# -------------------------------------------------------------------------
subtrees <- get_subtrees(
  dend = dend1,
  h = 0
)
num_labs <- sapply(subtrees, function(x) length(labels(x)))
test_that("get_subtrees works, h=0", {
  expect_equal(
    num_labs,
    rep(1, 32)
  )
})
# -------------------------------------------------------------------------
subtrees <- get_subtrees(
  dend = dend1,
  h = 500
)
num_labs <- sapply(subtrees, function(x) length(labels(x)))
test_that("get_subtrees works, h=500", {
  expect_equal(
    num_labs,
    32
  )
})
