
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


a <- DBI::dbListFields(z2, "metaData")
test_that("new DB has meta table", {
  expect_equal("metaData", 
               DBI::dbListTables(z2))
  
 expect_identical("Strain_IDGenbank_AccessionNCBI_TaxIDKingdomPhylumClassOrderFamilyGenusSpeciesMALDI_MatrixDSM_Agar_MediaCultivation_Temp_CelsiusCultivation_Time_DaysCultivation_OtherUserUser_ORCIDPI_FirstName_LastNamePI_ORCIDdna_16S",
                   paste0(a, collapse = ""))
  
})




