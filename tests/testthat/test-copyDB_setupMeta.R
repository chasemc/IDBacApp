
a <- system.file("extdata/sqlite", package = "IDBacApp")
z1 <- IDBacApp::createPool("sample", 
                           a)[[1]]
z1 <- pool::poolCheckout(z1)



a2 <- tempfile()
z2 <- IDBacApp::createPool(basename(a2), 
                           dirname(a2))[[1]]
z2 <- pool::poolCheckout(z2)




DBI::dbListTables(z2)



test_that("new DB is empty", {
  expect_equal(character(0), 
               DBI::dbListTables(z2))
})


IDBacApp::copyDB_setupMeta(z2, z1)



test_that("new DB has meta table", {
  expect_equal("metaData", 
               DBI::dbListTables(z2))
  
 expect_known_hash("266be4ae54",
                   DBI::dbListFields(z2, "metaData"))
  
})




