context("test-plotly_3d_scatter")

set.seed(42)

w <- cbind.data.frame(Dim1 = 1:10,
                     Dim2 = 1:10,
                     Dim3 = 1:10,
                     fac = 1:10,
                     nam = 1:10)
z <- IDBacApp::plotly_3d_scatter(w, "sd")

# not great test
test_that("multiplication works", {
 testthat::expect_equal(class(z), c("plotly", "htmlwidget"))
  })
