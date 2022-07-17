

a <- tempdir()
a <- file.path(a, "hello")
dir.create(a)
a <- file.path(a, "hi.txt")
file.create(a)

writeLines("hi",
           con = a)

z <- getMicrotyperFiles(dirname(a))


test_that("Single file getMicrotyperFiles works", {
  
  expect_equal(length(z), 1L)
  expect_equal(basename(z), "hi.txt")
  
})


# "~hello" directory (should currently contain 1 txt file)
 a <- dirname(a)

dir.create(file.path(a, "shouldBeIgnored"))

# add second to ~hello 
a <- file.path(a, "hi2.txt")
file.create(a)

writeLines("hi",
           con = a)

z <- getMicrotyperFiles(dirname(a))



test_that("Multiple file getMicrotyperFiles works", {
  
  expect_equal(length(z), 2L)
  expect_identical(basename(z), c("hi.txt", "hi2.txt"))
  
})


test_that("getMicrotyperFiles NULL", {
  
  expect_warning(getMicrotyperFiles(NULL), NULL)  
})


test_that("getMicrotyperFiles multiple", {
  
  expect_warning(getMicrotyperFiles(c(1,2)), NULL)  
})

