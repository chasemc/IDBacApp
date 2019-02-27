context("test-databasetransfer")

a1 <- tempfile()
b1 <- IDBacApp::createPool(basename(a1),
                           gsub("\\\\", "/", dirname(a1)))
bb1 <- pool::poolCheckout(b1[[1]])
bb1path <- gsub("\\\\", "/", bb1@dbname)

a2 <- tempfile()
b2 <- IDBacApp::createPool(basename(a2),
                           gsub("\\\\", "/", dirname(a2)))
bb2 <- pool::poolCheckout(b2[[1]])
bb2path <- gsub("\\\\", "/", bb2@dbname)



test_that("SQL warning on attach", {
  expect_warning(IDBacApp::copyDB_dbAttach(bb1path, bb2))
})

z2 <- DBI::dbGetQuery(bb2, "PRAGMA database_list;")

# -------------------------------------------------------------------------

old <- z2$file[[1]]
new <- z2$file[[3]]

test_that("SQL db was attached", {
  expect_identical(basename(old), paste0(basename(a2), ".sqlite"))
  expect_identical(basename(new), paste0(basename(a1), ".sqlite"))
})




pool::poolReturn(bb1)
pool::poolReturn(bb2)
pool::poolClose(b1[[1]])
pool::poolClose(b2[[1]])


# Setup new db ------------------------------------------------------------

a1 <- tempfile()
b1 <- IDBacApp::createPool(basename(a1),
                           gsub("\\\\", "/", dirname(a1)))
bb1 <- pool::poolCheckout(b1[[1]])
bb1path <- gsub("\\\\", "/", bb1@dbname)

a2 <- system.file(file.path("extdata", "sqlite","sample.sqlite"), package = 'IDBacApp')
b2 <- IDBacApp::createPool(basename(a2),
                           gsub("\\\\", "/", dirname(a2)))
bb2 <- pool::poolCheckout(b2[[1]])
bb2path <- gsub("\\\\", "/", bb2@dbname)



test_that("Provided and test sqlite DBs have correct tables", {
  expect_identical(DBI::dbListTables(bb2), c("IndividualSpectra", "XML", "metaData", "version"))
  expect_identical(DBI::dbListTables(bb1), character(0))  
})


# metaData setup -------------------------------------------------------

IDBacApp::copyDB_setupMeta(newDBconn = bb1,
                           existingDBconn = bb2,
                           arch = IDBacApp::sqlTableArchitecture(1)
                           )
a <- DBI::dbGetQuery(bb1,"SELECT * FROM metaData")

test_that("Correctly transferred 'metaData' table", {
  expect_identical(DBI::dbListTables(bb2), c("IndividualSpectra", "XML", "metaData", "version"))
  expect_identical(DBI::dbListTables(bb1), c("metaData"))
  expect_identical(nrow(a), 1L)
  expect_identical(ncol(a), 20L)
  expect_identical(sum(is.na(a)), 20L)
  
})

# XML setup ------------------------------------------------------------


IDBacApp::copyDB_setupXML(newDBconn = bb1,
                           existingDBconn = bb2,
                           arch = IDBacApp::sqlTableArchitecture(1)
)
a <- DBI::dbGetQuery(bb1,"SELECT * FROM XML")

test_that("Correctly setup 'XML' table", {
  expect_identical(DBI::dbListTables(bb2), c("IndividualSpectra", "XML", "metaData", "version"))
  expect_identical(DBI::dbListTables(bb1), c("metaData", "XML"))
  expect_identical(nrow(a), 1L)
  expect_identical(ncol(a), 8L)
  expect_identical(sum(is.na(a)), 8L) 
  
})


# IndividualSpectra -------------------------------------------------------


IDBacApp::copyDB_setupIndividualSpectra(newDBconn = bb1,
                                        existingDBconn = bb2,
                                        arch = IDBacApp::sqlTableArchitecture(1)
)
a <- DBI::dbGetQuery(bb1,"SELECT * FROM XML")

test_that("Correctly setup 'IndividualSpectra' table", {
  expect_identical(DBI::dbListTables(bb2), c("IndividualSpectra", "XML", "metaData", "version"))
  expect_identical(DBI::dbListTables(bb1), c("metaData", "XML", "IndividualSpectra"))
  expect_identical(nrow(a), 1L)
  expect_identical(ncol(a), 8L)
  expect_identical(sum(is.na(a)), 8L) 
  
})


