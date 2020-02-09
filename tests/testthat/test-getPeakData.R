a <- tempdir()
sql_path <- file.path(a,
                      "idbac_tests",
                      "sqlite", fsep = "/")
sql_path <- normalizePath(sql_path,
                          "/")
pool <- idbac_connect("testdb",
                      sql_path)[[1]]

prot <- idbac_get_peaks(pool = pool,
                     sampleIDs = c("A", "B"),
                     protein = T) 

small <- idbac_get_peaks(pool = pool,
                        sampleIDs = c("A", "B"),
                        protein = F) 

test_that("getPeakData works", {

  expect_equal(length(prot), 2)
  expect_equal(length(small), 2)
  expect_equal(unname(lengths(prot)), c(4,4))
  expect_equal(unname(lengths(small)), c(4,4))
  
  expect_known_hash(prot, "f91b054766")
  expect_known_hash(small, "2a2b945389")
  
})
