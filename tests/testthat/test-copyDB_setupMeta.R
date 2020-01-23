download.file("ftp://massive.ucsd.edu/MSV000084291/raw/data/idbac_experiment_file.sqlite", temp, mode = "wb")
z1 <- IDBacApp::idbac_connect(tools::file_path_sans_ext(basename(temp)), dirname(temp))
z1=z1[[1]]
DBI::dbListTables(z1)
IDBacApp::update_database_version(z1)


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


a <- DBI::dbListFields(z2, "metadata")
test_that("new DB has meta table", {
  expect_equal("metadata", 
               DBI::dbListTables(z2))
  
 expect_identical("strain_idgenbank_accessionncbi_taxidkingdomphylumclassorderfamilygenusspeciesmaldi_matrixdsm_cultivation_mediacultivation_temp_celsiuscultivation_time_dayscultivation_otheruser_firstname_lastnameuser_orcidpi_firstname_lastnamepi_orciddna_16s",
                   paste0(a, collapse = ""))
  
})




