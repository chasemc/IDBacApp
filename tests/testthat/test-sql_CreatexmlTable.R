a2 <- tempfile()
z2 <- IDBacApp::createPool(basename(a2), 
                           dirname(a2))[[1]]
z3 <- pool::poolCheckout(z2)

DBI::dbListTables(z3)


test_that("new DB is empty", {
  expect_equal(character(0), 
               DBI::dbListTables(z3))
})


IDBacApp::sql_create_xml_table(z3)
a <- DBI::dbListFields(z3, "xml")

test_that("new DB has xml table", {
  expect_equal("xml", 
               DBI::dbListTables(z3))
  
  expect_identical("xml_hashxmlmanufacturermodelionizationanalyzerdetectorinstrument_metafile",
                    paste0(a, collapse = ""))
  
})

