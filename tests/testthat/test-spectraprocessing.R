context("test-spectraprocessing")


#----
extData <- system.file("extdata", package="IDBacApp")
extData <- file.path(extData, "temp_mz")
extData <- list.files(extData, full.names = TRUE)

dbcon <- tempfile("a", fileext = ".sqlite")

dbcon <- pool::dbPool(drv = RSQLite::SQLite(),
                      dbname = dbcon
)

for(i in seq_along(extData)){
  spectraProcessingFunction(rawDataFilePath = extData[[i]],
                            sample_ID = c("a", "12", "s#4", "..l")[[i]] ,
                            userDBCon = dbcon)
}

dbQuery <- glue::glue_sql("SELECT *
                                  FROM ({tab*})",
                          tab = "metaData",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)
a <- c("a, 12, s#4, ..l, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA")
a <- matrix(strsplit(a, ", ")[[1]], 4, 20)
a[which(a == "NA")] <- NA
colnames(a) <- c("Strain_ID","Genbank_Accession","NCBI_TaxID","Kingdom","Phylum","Class","Order","Family","Genus","Species","MALDI_Matrix","DSM_Agar_Media","Cultivation_Temp_Celsius","Cultivation_Time_Days","Cultivation_Other","User","User_ORCID","PI_FirstName_LastName","PI_ORCID","dna_16S")
a<-as.data.frame(a, stringsAsFactors = FALSE)


meta <- dbQuery
metaExpected <- a


#----


dbQuery <- glue::glue_sql("SELECT *
                                  FROM ({tab*})",
                          tab = "XML",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)

xmlshaExpected <- list(mzMLSHA = "a087e884535da97f11d7b5adc5848d44ae6497fd",
                       XML = "e96b1f822f99b94018a304875516313c46df2a5e",
                       manufacturer = "f82e0523bc1903dd006bdc457b78a4c3cdd48427",
                       model = "f82e0523bc1903dd006bdc457b78a4c3cdd48427",
                       ionisation = "f82e0523bc1903dd006bdc457b78a4c3cdd48427",
                       analyzer = "f82e0523bc1903dd006bdc457b78a4c3cdd48427",
                       detector = "f82e0523bc1903dd006bdc457b78a4c3cdd48427",
                       Instrument_MetaFile = "d0f948cd76120cbb7b0be2f84f9a97e029e6449e")
xmlsha <- lapply(dbQuery, function(x) digest::digest(x, "sha1"))
#----


dbQuery <- glue::glue_sql("SELECT *
                                  FROM ({tab*})",
                          tab = "IndividualSpectra",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)
indspecExpected <- list(spectrumSHA= "50e06e324d368e5d9ff774acd8f2eb934dd732ea",
                        mzMLSHA = "c2a982731db1a1b0ca9aefa87f503c46d61fcaf9",
                        Strain_ID = "e837ca2213a09141d0edc4c4da6a7a184da6ac11",
                        proteinPeaks = "6d9464176e2169743ceceb13385a9d1206ae895c",
                        proteinSpectrum = "316752dc0afcb2fba977a1c710fd81d86a8af5d1",
                        smallMoleculePeaks = "c06de05ef8610b887af9296abfdb537b91142292",
                        smallMoleculeSpectrum = "1ac82858bd5ba597297a05b715dd55cb0c8224fd")



indspec <- lapply(dbQuery, function(x) digest::digest(x, "sha1"))


#----







sqltables <- c("IndividualSpectra", "XML", "metaData")


test_that("Multiple-Samples: sql tables exist", {
  expect_identical(DBI::dbListTables(dbcon), sqltables)
})

test_that("Multiple-Samples: sql metadata table contains expected", {
  expect_identical(lapply(meta, as.character), lapply(metaExpected, as.character))
  
})


test_that("Multiple-Samples: sql IndividualSpectra table contains expected", {
  expect_identical(indspecExpected, indspec)  
})


test_that("Multiple-Samples: sql XML table contains expected", {
  expect_identical(xmlshaExpected, xmlsha)
  
})




#---- Only one sample



#----
extData <- system.file("extdata", package="IDBacApp")
extData <- file.path(extData, "temp_mz")
extData <- list.files(extData, full.names = TRUE)[[1]]

dbcon <- tempfile("b", fileext = ".sqlite")

dbcon <- pool::dbPool(drv = RSQLite::SQLite(),
                      dbname = dbcon
)


  spectraProcessingFunction(rawDataFilePath = extData,
                            sample_ID = c("a"),
                            userDBCon = dbcon)


dbQuery <- glue::glue_sql("SELECT *
                          FROM ({tab*})",
                          tab = "metaData",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)
a <- matrix(c("a", rep(NA, 19)),1,20)
colnames(a) <- c("Strain_ID","Genbank_Accession","NCBI_TaxID","Kingdom","Phylum","Class","Order","Family","Genus","Species","MALDI_Matrix","DSM_Agar_Media","Cultivation_Temp_Celsius","Cultivation_Time_Days","Cultivation_Other","User","User_ORCID","PI_FirstName_LastName","PI_ORCID","dna_16S")
a<-as.data.frame(a, stringsAsFactors = FALSE)


meta <- dbQuery
metaExpected <- a


#----


dbQuery <- glue::glue_sql("SELECT *
                          FROM ({tab*})",
                          tab = "XML",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)

xmlshaExpected <- list(mzMLSHA = "ef83ce2f0835b178f65360ed34f9a78298039060",
                       XML = "16516486f32da7aa090e3d329630fb5ae0183108",
                       manufacturer = "bc0c7f4a065200471fd7a299607b4ace6a4c6f6c",
                       model = "bc0c7f4a065200471fd7a299607b4ace6a4c6f6c",
                       ionisation = "bc0c7f4a065200471fd7a299607b4ace6a4c6f6c",
                       analyzer = "bc0c7f4a065200471fd7a299607b4ace6a4c6f6c",
                       detector = "bc0c7f4a065200471fd7a299607b4ace6a4c6f6c",
                       Instrument_MetaFile = "1ee8f3a54a96867299f06549415fd0b0ff64c7e3")
xmlsha <-lapply(dbQuery, function(x) digest::digest(x, "sha1"))
#----


dbQuery <- glue::glue_sql("SELECT *
                                  FROM ({tab*})",
                          tab = "IndividualSpectra",
                          .con = dbcon)

conn <- pool::poolCheckout(dbcon)
dbQuery <- DBI::dbSendQuery(conn, dbQuery)
dbQuery <- DBI::dbFetch(dbQuery)
indspecExpected <- list(spectrumSHA = "5eeb726c1bf58ff6ea58159e3c9d896709fcb8ee",
                        mzMLSHA = "11a12c3581035f2b2e0f157c38101fd47f8dff34",
                        Strain_ID = "2408c16df2240c92777442a3a67e8ab4ec0edc21",
                        proteinPeaks = "99c76a82865e7203c6c0f80a7d9bd606e505173f",
                        proteinSpectrum = "d851e3d74169d7a5cc207e4cde47afd72c00f24e",
                        smallMoleculePeaks = "83b0a4fdce6f728c8fa9b9f6e99a801b51cb1448",
                        smallMoleculeSpectrum = "74cbbba135212780daec4c5c298e86fc18b18c5e")



indspec <-lapply(dbQuery, function(x) digest::digest(x, "sha1"))




sqltables <- c("IndividualSpectra", "XML", "metaData")


test_that("One-Sample: sql tables exist", {
  expect_identical(DBI::dbListTables(dbcon), sqltables)
})

test_that("One-Sample: sql metadata table contains expected", {
  expect_identical(lapply(meta, as.character), lapply(metaExpected, as.character))
  
})


test_that("One-Sample: sql IndividualSpectra table contains expected", {
  expect_identical(indspecExpected, indspec)  
})


test_that("One-Sample: sql XML table contains expected", {
  expect_identical(xmlshaExpected, xmlsha)
  
})

