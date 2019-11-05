   
a <- "[1.022515,2.021542,2]"

b <- IDBacApp::fastJsonVectorParse(a)


test_that("multiplication works", {
  expect_identical(b, c(1.022515,2.021542,2))
})




readxl::