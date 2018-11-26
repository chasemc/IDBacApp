context("test-colored_dots")
# not a good test, but at least checks if it has changed
a<-digest::digest(deparse(IDBacApp::colored_dots),"sha1")

test_that("has colored dots changed", {
  expect_equal(a, "3ae530df063facb77bbc9f666233d7ed2ad1bf96")
})


