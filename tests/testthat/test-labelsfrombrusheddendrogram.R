context("test-labelsfrombrusheddendrogram")

a <- as.dendrogram(hclust(dist(mtcars)))

test_that("multiplication works", {
  expect_identical(
    labelsFromBrushedDendrogram(
      dendrogram = a,
      brushYmin = 1,
      brushYmax = 5
    ),
    c("Chrysler Imperial", "Cadillac Fleetwood", "Lincoln Continental")
  )
})
