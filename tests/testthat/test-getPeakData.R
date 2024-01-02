a <- tempdir()
sql_path <- file.path(a,
  "idbac_tests",
  "sqlite",
  fsep = "/"
)
sql_path <- normalizePath(
  sql_path,
  "/"
)
pool <- idbac_connect(
  "testdb",
  sql_path
)[[1]]
prot <- idbac_get_peaks(
  pool = pool,
  sampleIDs = c("D"),
  minFrequency = 0,
  minNumber = NA,
  lowerMassCutoff = 3000,
  upperMassCutoff = 15000,
  minSNR = 3,
  tolerance = 0.002,
  type = "protein",
  mergeReplicates = F,
  method = "strict"
)
small <- idbac_get_peaks(
  pool = pool,
  sampleIDs = c("D"),
  minFrequency = 0,
  minNumber = NA,
  lowerMassCutoff = 300,
  upperMassCutoff = 1500,
  minSNR = 3,
  tolerance = 0.002,
  type = "small",
  mergeReplicates = TRUE,
  method = "strict"
)
test_that("getPeakData works", {
  expect_equal(length(prot), 1)
  expect_equal(length(small), 1)
  expect_known_hash(prot, "1b98afbf42")
  expect_known_hash(small, "0d0d0cd8e5")
})
