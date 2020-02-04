
dend1 <- as.dendrogram(hclust(dist(mtcars)))

get_one_subtree

subtrees <- get_subtrees(dend = dend1,
                         h = 200)

num_labs <- sapply(subtrees, function(x) length(labels(x)))

test_that("get_subtrees works, h=200", {
  expect_equal(num_labs,
               c(16,7,8,1))
})

# -------------------------------------------------------------------------

subtrees <- get_subtrees(dend = dend1,
                         h = 0)
num_labs <- sapply(subtrees, function(x) length(labels(x)))

test_that("get_subtrees works, h=0", {
  expect_equal(num_labs,
               rep(1,32))
})

# -------------------------------------------------------------------------

subtrees <- get_subtrees(dend = dend1,
                         h = 500)
num_labs <- sapply(subtrees, function(x) length(labels(x)))

test_that("get_subtrees works, h=500", {
  expect_equal(num_labs,
               32)
})

