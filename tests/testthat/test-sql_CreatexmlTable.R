a2 <- tempfile()
z2 <- IDBacApp::createPool(basename(a2), 
                           dirname(a2))[[1]]
z3 <- pool::poolCheckout(z2)




DBI::dbListTables(z3)


test_that("new DB is empty", {
  expect_equal(character(0), 
               DBI::dbListTables(z3))
})


IDBacApp::sql_CreatexmlTable(z3)
a <- DBI::dbListFields(z3, "XML")

test_that("new DB has XML table", {
  expect_equal("XML", 
               DBI::dbListTables(z3))
  
  expect_identical("XMLHashXMLmanufacturermodelionizationanalyzerdetectorInstrument_MetaFile",
                    paste0(a, collapse = ""))
  
})


suppressWarnings(gc())