context("findMSconvert")


a <- tempdir()
a <- file.path(a , "msconvert.exe")
file.create(a)

b <- IDBacApp::findMSconvert(a)



test_that("findMSconvert returns msconvert.exe path if found", {
  expect_equal(a, b)
})

test_that("findMSconvert returns messages", {
  expect_message(IDBacApp::findMSconvert(), "Given path to msconvert didn't work, trying to auto-find in Programs")
})
