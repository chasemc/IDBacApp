context("test-utils")
set.seed(42)

# serial ------------------------------------------------------------------

a <- list(mass = 1:3, 
         intensity = 1:3, 
         snr = 1:3)


b <- IDBacApp::serial(a)


test_that("IDBacApp::serial() works", {
  expect_identical(class(b), "json")
  expect_identical(as.character(b),
                   "{\"mass\":[1,2,3],\"intensity\":[1,2,3],\"snr\":[1,2,3]}")
})



# deserial ----------------------------------------------------------------

z <- IDBacApp::deserial(b)

test_that("IDBacApp::deserial() works", {
  expect_identical(class(z), "list")
  expect_identical(a, z)
})




# compress/decompress -----------------------------------------------------

a <- IDBacApp::compress(base::serialize(mtcars, NULL))
b <- IDBacApp::decompress(a)
b <- unserialize(b)

test_that("compress() deserial() works", {
  expect_identical(b, mtcars)
})


# chartoRawtoCompressed ---------------------------------------------------

a <- IDBacApp::chartoRawtoCompressed("Hello", 0) 
a <- paste0(a, collapse="")
b <- "9f9b683c0040000001000000030000000500000000000000000000000000000030000000000000003e0000000000000028b52ffd200529000048656c6c6f"

test_that("compress() deserial() works", {
  expect_identical(a, b)
    })



# serializeXML ------------------------------------------------------------

a <- MALDIquant::createMassSpectrum(mass=1:5, intensity=1:5)
b <- tempfile(fileext = ".mzML")
MALDIquantForeign::exportMzMl(a, b)

z <- IDBacApp::serializeXML(b)

test_that("serializeXML", {
  #hash doesn't work
  expect_equal(class(z), "raw")
})





# cleanWSpace -------------------------------------------------------------

a <- IDBacApp::cleanWSpace(" hello% i _am")

test_that("cleanWSpace works", {
  expect_identical(a, "hello%_i_am")
})



