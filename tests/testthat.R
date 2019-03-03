library(testthat)
library(IDBacApp)

Sys.setenv("R_TESTS" = "")

test_check("IDBacApp")
