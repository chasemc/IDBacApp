
temp <- tempfile()

con <- pool::dbPool(drv = RSQLite::SQLite(),
                    dbname = ":memory:")

IDBacApp::sqlCreate_version(con)

test_that("create database 'version' table", {
  expect_true("version" %in% DBI::dbListTables(con))
  
  expect_equal(dim((DBI::dbGetQuery(con, "SELECT * FROM version"))),
               c(1,2))
  
  expect_equal(colnames(DBI::dbGetQuery(con, "SELECT * FROM version")),
               c("IDBacVersion", "rVersion"))
  
  expect_equal(DBI::dbGetQuery(con, "SELECT * FROM version")[[1]],
               getNamespaceVersion("IDBacApp")[[1]])
})


# insertLocale ------------------------------------------------------------

IDBacApp::insertLocale(con)

test_that("create database 'locale' table", {
  expect_true("locale" %in% DBI::dbListTables(con))
  
  expect_equal(dim((DBI::dbGetQuery(con, "SELECT * FROM locale"))),
               c(1,1))
  expect_equal(class(DBI::dbGetQuery(con, "SELECT * FROM locale")[[1]]),
               "character")
  expect_equal(colnames(DBI::dbGetQuery(con, "SELECT * FROM locale")),
               "locale")
  expect_true(nchar(DBI::dbGetQuery(con, "SELECT * FROM locale")[[1]]) > 10)
})



# createMetaSQL -----------------------------------------------------------

IDBacApp::createMetaSQL(sampleID = c("samIam", "greenEggs"),
                        userDBCon = con)

test_that("create database 'metadata' table", {
  expect_true("metaData" %in% DBI::dbListTables(con))
  
  expect_equal( dim((DBI::dbGetQuery(con, "SELECT * FROM metadata"))),
                c(2, 20))
})

test_that("create database 'metadata' table; check columns", {
  expect_equal(colnames(DBI::dbGetQuery(con, "SELECT * FROM metaData")),
               c("Strain_ID",
                 "Genbank_Accession",
                 "NCBI_TaxID",
                 "Kingdom",
                 "Phylum",
                 "Class",
                 "Order",
                 "Family",
                 "Genus",
                 "Species",
                 "MALDI_Matrix",
                 "DSM_Agar_Media",
                 "Cultivation_Temp_Celsius",
                 "Cultivation_Time_Days",
                 "Cultivation_Other",
                 "User",
                 "User_ORCID",
                 "PI_FirstName_LastName",
                 "PI_ORCID",
                 "dna_16S")
  )
})




test_that("create database 'metadata' table: sample name", {
  
  expect_equal(DBI::dbGetQuery(con, "SELECT * FROM metaData")[,1],
               c("samIam","greenEggs"))
})




# createXMLSQL ------------------------------------------------------------


mzML_con <- mzR::openMSfile(system.file(file.path("extdata",
                                                  "mzml",
                                                  "mzml.mzML"),
                                        package = "IDBacApp"),
                            backend = "pwiz")

result <- IDBacApp::createXMLSQL(rawDataFilePath = system.file(file.path("extdata",
                                                                         "mzml",
                                                                         "mzml.mzML"),
                                                               package = "IDBacApp"),
                                 userDBCon = con,
                                 mzML_con = mzML_con)


test_that("create database 'XML' table", {
  expect_true("XML" %in% DBI::dbListTables(con))
  expect_known_hash(result, 
                    "233c42a5d9")
})




# createSpectraSQL --------------------------------------------------------
scanNumber <- nrow(mzR::header(mzML_con))

createSpectraSQL(mzML_con = mzML_con, 
                 scanNumber = 1,
                 userDBCon = con,
                 sampleID = "hello",
                 XMLinfo = result,
                 smallRangeEnd = 6000,
                 acquisitionInfo = NULL)
