context("test-insertlocale")


con <- pool::dbPool(drv = RSQLite::SQLite(), dbname = ":memory:")

IDBacApp::sql_fill_locale_table(con)

a <- DBI::dbGetQuery(con, 
                     "SELECT *
                  FROM locale"
                     
)

test_that("multiplication works", {
  expect_equal(DBI::dbListTables(con), "locale")
  expect_equal(DBI::dbListFields(con, "locale"), "locale")
  expect_gt(nchar(a), 10)
  })

