mzML_con <- mzR::openMSfile(system.file(file.path("extdata",
                                                  "mzml",
                                                  "mzml.mzML"),
                                        package = "IDBacApp"),
                            backend = "pwiz")


createSpectraSQL(mzML_con, 
                 scanNumber,
                 userDBCon,
                 sampleID,
                 XMLinfo,
                 smallRangeEnd = 6000,
                 acquisitionInfo)





test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
