
working_directory <- file.path(tempdir(), "example_dir")
suppressWarnings(dir.create(file.path(tempdir(), "example_dir")))

# URL of example file
ex_url <- "ftp://massive.ucsd.edu/MSV000084291/raw/data/idbac_experiment_file.sqlite"
# Path of created file 
ex_path <- normalizePath(file.path(working_directory, 
                                   "idbac_experiment_file.sqlite"),
                         winslash = "/",
                         mustWork = FALSE)
# Download example file ("wb" is important here)
download.file(url = ex_url,
              destfile = ex_path,
              mode = "wb")

example_pool <- IDBacApp::createPool(fileName = "idbac_experiment_file",
                                     filePath = working_directory)[[1]]

my_plot <- IDBacApp::assembleMirrorPlots(sampleID1 = "172-7",
                                         sampleID2 = "172-10",
                                         peakPercentPresence = 0.7,
                                         lowerMassCutoff = 3000,
                                         upperMassCutoff = 15000,
                                         minSNR = 4, 
                                         tolerance = 0.002,
                                         pool1 = example_pool,
                                         pool2 = example_pool)

z <- IDBacApp::mirrorPlot(my_plot)

test_that("multiplication works", {
  expect_known_hash(as.list(my_plot), "52eba8dabd")
  expect_visible(z)
  expect_equal(class(z), c("plotly", "htmlwidget"))
})
