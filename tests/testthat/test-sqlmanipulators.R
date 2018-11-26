context("test-sqlmanipulators")

extData <- system.file("extdata", package="IDBacApp")
extData <- file.path(extData, "sql_example", "test.sqlite")



dbcon <- pool::dbPool(drv = RSQLite::SQLite(),
                      dbname = extData
)


dbQuery <- glue::glue_sql("SELECT *
                          FROM ({tab*})",
                          tab = "IndividualSpectra",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)


a <- getProteinPeakData(db = dbcon,
                   fileshas = c("d0f9f30f011442f1c039beb94ea5d4469c094ed4",
                                "183ee8a0997ec242373b1425b45da289d61e88e0"))

# sha are fpr sepctra from two different samples so can test percent presence
b <- collapseProteinReplicates(db = dbcon,
                               fileshas = c("d0f9f30f011442f1c039beb94ea5d4469c094ed4",
                                            "183ee8a0997ec242373b1425b45da289d61e88e0"),
                               proteinPercentPresence = 50,
                               lowerMassCutoff = 0,
                               upperMassCutoff = 10000)

nms <- slotNames(b)
names(nms) <- nms
bb <- lapply(nms, slot, object=b)
bb <- digest::digest(bb, "sha1")


test_that("protein collapse", {
  expect_identical(bb, "13fa81ff8229625a7742301f900d33b6baebc055")
})



sampleNames <- getAllStrain_IDsfromSQL(databaseConnection = dbcon,
                           table = "IndividualSpectra")

test_that("protein collapse", {
  expect_identical(sampleNames, c("1","11","matrix", "ten"))
})

