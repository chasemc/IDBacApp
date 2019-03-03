context("test-dbcreators")

set.seed(42)
# Create version ----------------------------------------------------------


a <- basename(tempfile())
b <- tempdir()

pool <- IDBacApp::createPool(a, b)[[1]]

con <- pool::poolCheckout(pool)


test_that("no Version", {
  expect_false(DBI::dbExistsTable(con, "version"))
})

IDBacApp::sqlCreate_version(con)
a <-  DBI::dbGetQuery(con, "SELECT * FROM version")

test_that("Version database table creation", {
  expect_true(DBI::dbExistsTable(con, "version"))
  expect_identical(ncol(a), 1L)
  expect_identical(nrow(a), 1L)
})


# Create Meta -------------------------------------------------------------



test_that("no metaData", {
  expect_false(DBI::dbExistsTable(con, "metaData"))
})

IDBacApp::createMetaSQL(sampleID = c("hello", "hello", "hi"),
                        userDBCon = con)



a <-  DBI::dbGetQuery(con, "SELECT * FROM metaData")
b <- a[, -which(colnames(a) == "Strain_ID")]
b <- all(is.na(b))

test_that("metaData database table creation", {
  expect_true(DBI::dbExistsTable(con, "metaData"))
  expect_identical(a$Strain_ID, c("hello", "hi"))
  expect_true(b)
})



# Create XML --------------------------------------------------------------

test_that("no metaData", {
  expect_false(DBI::dbExistsTable(con, "XML"))
})

path <- system.file(file.path("extdata", "mzML","mzml.mzML"), package = 'IDBacApp')

mzcon <- mzR::openMSfile(path)

out <- IDBacApp::createXMLSQL(rawDataFilePath = path,
                       userDBCon = con,
                       mzML_con = mzcon)

IDBacApp::createXMLSQL(rawDataFilePath = path,
                       userDBCon = con,
                       mzML_con = mzcon)

a <-  DBI::dbGetQuery(con, "SELECT * FROM XML")
b <- a$XML[[1]]
b <- a$XML[[1]]

test_that("XML database table creation", {
  expect_true(DBI::dbExistsTable(con, "XML"))
  expect_identical(nrow(a), 1L)
  expect_identical(ncol(a), 8L)
  expect_identical(class(b), "raw")
  expect_identical(out$mzMLHash, "3b3a0e4975b86523")
  
})





# Create Individual -------------------------------------------------------




pool::poolReturn(con)
pool::poolClose(pool)