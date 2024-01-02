# create idbac database ---------------------------------------------------
a <- tempdir()
sql_path <- file.path(a,
  "idbac_tests",
  "sqlite",
  fsep = "/"
)
dir.create(sql_path)
sql_path <- normalizePath(
  sql_path,
  "/"
)
test_that("idbac_create() gives path", {
  expect_equal(
    idbac_create(
      "testdb",
      sql_path
    ),
    file.path(
      sql_path,
      "testdb.sqlite"
    )
  )
})

pool <- idbac_connect(
  "testdb",
  sql_path
)

test_that("idbac_create gives example db connection", {
  expect_equal(length(pool), 1)
  expect_true(inherits(pool[[1]], "Pool"))
  expect_equal(labels(pool), "testdb")
})
pool <- pool[[1]]

mzml_path <- file.path(a,
  "idbac_tests",
  "mzml",
  fsep = "/"
)
mzml_path <- normalizePath(
  mzml_path,
  "/"
)
mzml_path <- list.files(mzml_path, full.names = T)

test_that("db_from_mzml gives messages", {
  expect_message(
    suppressWarnings({
      db_from_mzml(
        mzFilePaths = mzml_path,
        sampleIds = LETTERS[1:4],
        idbacPool = pool,
        acquisitionInfo = NULL,
        halfWindowSize = 2
      )
    })
  )
})

mass_hashes <- pool::poolWithTransaction(
  pool,
  function(conn) {
    statement <- glue::glue(
      "SELECT spectrum_mass_hash
                                FROM spectra"
    )
    query <- DBI::dbSendStatement(
      statement = statement,
      con = conn
    )
    selectedMeta <- DBI::dbFetch(query)
    DBI::dbClearResult(query)
    return(selectedMeta)
  }
)
test_that("expected masses are present", {
  expect_equal(
    sort(unique(unlist(mass_hashes))),
    c("08caa9e63bcce83c", "90d91118298abb99")
  )
})
