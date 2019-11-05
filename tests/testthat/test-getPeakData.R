getPeakData

working_directory <- file.path(tempdir(), "example_dir")
dir.create(file.path(tempdir(), "example_dir"))

# URL of example file
ex_url <- "https://github.com/chasemc/IDBacApp/releases/download/0.0.15.2/test.sqlite"
# Path of created file 
ex_path <- normalizePath(file.path(working_directory, 
                                   "test.sqlite"),
                         winslash = "/",
                         mustWork = FALSE)
# Download example file ("wb" is important here)
download.file(url = ex_url,
              destfile = ex_path,
              mode = "wb")

example_pool <- IDBacApp::createPool(fileName = "test",
                                     filePath = working_directory)[[1]]


a <- IDBacApp::getPeakData(example_pool,
                      c("172-1", "172-7"),
                      protein = T)
b <- IDBacApp::getPeakData(example_pool,
                           c("172-1"),
                           protein = T)
d <- IDBacApp::getPeakData(example_pool,
                           c("172-1", "172-10"),
                           protein = F)

test_that("getPeakData works", {
  expect_known_hash(a,"b356ed97be")
  expect_known_hash(b,"4ef775f3b7")
  expect_known_hash(d,"091c324efb")
})

test_that("getPeakData errors correctly", {
  expect_error(IDBacApp::getPeakData(example_pool, list("172-1"),protein = T))
})
