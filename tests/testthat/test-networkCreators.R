context("test-networkfromdf")
a <- cbind.data.frame(Source = c("a", "b", "a"), Target = c(1.1, 1.1, 2.2), Weight = c(1, 2, 1))
a <- networkFromDF(a)
test_that("network from data frame works", {
  expect_equal(class(a), "igraph")
  expect_identical(
    attributes(igraph::V(a))[c("names", "class")],
    list(
      names = c("a", "b", "1.1", "2.2"),
      class = "igraph.vs"
    )
  )
})

# modularity --------------------------------------------------------------

z <- modularityClustering(a)

test_that("modularity scoring works", {
  expect_equal(class(a), "igraph")
  expect_identical(
    igraph::V(z)$color,
    c("#E69F00", "#000000", "#000000", "#E69F00")
  )
})
