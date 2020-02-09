context("test-utils")
set.seed(42)

# serial ------------------------------------------------------------------


b <- serial(list(mass = 1:10))


test_that("serial() works", {
  expect_identical(class(b), "json")
  expect_identical(as.character(b),
                   "{\"mass\":[1,2,3,4,5,6,7,8,9,10]}")
})



# compress/decompress -----------------------------------------------------

a <- compress(base::serialize(mtcars, NULL))
b <- decompress(a)
b <- unserialize(b)

test_that("compress() deserial() works", {
  expect_identical(b, mtcars)
})


# chartoRawtoCompressed ---------------------------------------------------

a <- chartoRawtoCompressed("Hello", 0) 
a <- paste0(a, collapse="")
b <- "9f9b683c0040000001000000030000000500000000000000000000000000000030000000000000003e0000000000000028b52ffd200529000048656c6c6f"

test_that("compress() deserial() works", {
  expect_identical(a, b)
    })



# serializeXML ------------------------------------------------------------

a <- MALDIquant::createMassSpectrum(mass=1:5, intensity=1:5)
b <- tempfile(fileext = ".mzML")
MALDIquantForeign::exportMzMl(a, b)

z <- serializeXML(b)

test_that("serializeXML", {
  #hash doesn't work
  expect_equal(class(z), "raw")
})





# cleanWSpace -------------------------------------------------------------

a <- sanitize(" hello% i _am")

test_that("cleanWSpace works", {
  expect_identical(a, "_hello%_i_am")
})



