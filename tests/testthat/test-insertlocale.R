context("test-insertlocale")


con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

IDBacApp::insertLocale(con)

a <- DBI::dbGetQuery(con, 
                     "SELECT *
                  FROM locale"
                     
)

test_that("multiplication works", {
  expect_equal(DBI::dbListTables(con), "locale")
  expect_equal(DBI::dbListFields(con, "locale"), "locale")
  expect_gt(nchar(a), 10)
  })

