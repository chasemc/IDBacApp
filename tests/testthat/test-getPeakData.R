a <- tempdir()
sql_path <- file.path(a,
                      "idbac_tests",
                      "sqlite", fsep = "/")
sql_path <- normalizePath(sql_path,
                          "/")
pool <- idbac_connect("testdb",
                      sql_path)[[1]]

prot <- idbac_get_peaks(pool = pool,
                        sampleIDs = c("D"),
                        minFrequency = 0,
                        minNumber = NA, 
                        lowerMassCutoff = 3000,
                        upperMassCutoff = 15000, 
                        minSNR = 3, 
                        tolerance = 0.002,
                        type = "protein",
                        mergeReplicates = F,
                        method = "strict")

small <- idbac_get_peaks(pool = pool,
                         sampleIDs = c("A", "B"),
                         minFrequency = 0,
                         minNumber = NA, 
                         lowerMassCutoff = 300,
                         upperMassCutoff = 1500, 
                         minSNR = 3, 
                         tolerance = 0.002,
                         type = "small",
                         mergeReplicates = TRUE,
                         method = "strict")

test_that("getPeakData works", {
  
  expect_equal(length(prot), 2)
  expect_equal(length(small), 2)
  expect_equal(unname(lengths(prot)), c(4,4))
  expect_equal(unname(lengths(small)), c(4,4))
  
  expect_known_hash(prot, "f91b054766")
  expect_known_hash(small, "2a2b945389")
  
})
