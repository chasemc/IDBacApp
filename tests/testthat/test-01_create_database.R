
# create idbac database ---------------------------------------------------

a <- tempdir()
sql_path <- file.path(a,
                      "idbac_tests",
                      "sqlite", fsep = "/")
dir.create(sql_path)
sql_path <- normalizePath(sql_path,
                          "/")

test_that("idbac_create() gives path", {
  expect_equal(idbac_create("testdb",
                                      sql_path), 
               file.path(sql_path,
                         "testdb.sqlite")
  )
})


pool <- idbac_connect("testdb",
                                sql_path)


test_that("idbac_create gives example db connection", {
  expect_equal(length(pool), 1)
  expect_true(inherits(pool[[1]], "Pool"))
  expect_equal(labels(pool), "testdb")
})

pool <- pool[[1]]



mzml_path <- file.path(a,
                       "idbac_tests",
                       "mzml",
                       fsep = "/")
mzml_path <- normalizePath(mzml_path,
                           "/")
mzml_path <- list.files(mzml_path, full.names = T)


test_that("process_mzml gives messages", {
  expect_message(
    suppressWarnings({
      process_mzml(mzFilePaths = mzml_path,
                   sampleIds = LETTERS,
                   idbacPool = pool,
                   acquisitionInfo = NULL)
    })
  )
})


mass_hashes <- pool::poolWithTransaction(pool, 
                                         function(conn){
                                           statement <- glue::glue(
                                             "SELECT spectrum_mass_hash 
                                FROM spectra"
                                           )
                                           query <- DBI::dbSendStatement(statement = statement,
                                                                         con = conn)
                                           selectedMeta <- DBI::dbFetch(query)
                                           DBI::dbClearResult(query)
                                           return(selectedMeta)
                                           
                                         })

test_that("expected masses are present", {
  expect_equal(sort(unique(unlist(mass_hashes))),
               c("6da07a39e4e6411e", "e68310493b583d60"))
})
