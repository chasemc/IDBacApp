a <- tempdir()
sql_path <- file.path(a,
                      "idbac_tests",
                      "sqlite", fsep = "/")
sql_path <- normalizePath(sql_path,
                          "/")
pool <- idbac_connect("testdb",
                      sql_path)[[1]]

my_plot <- assembleMirrorPlots(sampleID1 = "D",
                               sampleID2 = "D",
                               minFrequency = 0.7,
                               lowerMassCutoff = 3000,
                               upperMassCutoff = 15000,
                               minSNR = 4, 
                               tolerance = 0.002,
                               pool1 = pool,
                               pool2 = pool)

z <- mirrorPlot(my_plot)

test_that("mirror works", {
  expect_known_hash(as.list(my_plot), "113af21230")
  expect_visible(z)
  expect_equal(class(z), c("plotly", "htmlwidget"))
})
