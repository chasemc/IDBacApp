library(testthat)
library(IDBacApp)
Sys.setenv("R_TESTS" = "")
suppressWarnings({
  test_check("IDBacApp")
})
