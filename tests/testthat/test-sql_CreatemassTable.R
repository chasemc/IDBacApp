# 
# a <- system.file("extdata/sqlite", package = "IDBacApp")
# z1 <- createPool("sample", 
#                            a)[[1]]
# z1 <- pool::poolCheckout(z1)
# 
# 
# 
# a2 <- tempfile()
# z2 <- createPool(basename(a2), 
#                            dirname(a2))[[1]]
# z2 <- pool::poolCheckout(z2)
# 
# 
# 
# 
# DBI::dbListTables(z2)
# 
# 
# test_that("new DB is empty", {
#   expect_equal(character(0), 
#                DBI::dbListTables(z2))
# })
# 
# 
# sql_create_massindex_table(z2)
# a <- DBI::dbListFields(z2, "mass_index")
# 
# test_that("new DB has Mass table", {
#   expect_equal("mass_index", 
#                DBI::dbListTables(z2))
#   
#   expect_identical("spectrum_mass_hashmass_vector",
#                    paste0(a, collapse = ""))
#   
# })
# 
# 
# 
