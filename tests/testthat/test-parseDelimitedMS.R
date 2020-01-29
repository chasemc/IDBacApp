set.seed(42)
#Make some temporary directories to write to:

make_temp <- function(x){
  loc <- file.path(tempdir(), x)
  dir.create(loc)
  loc <- normalizePath(loc, "/")
  return(loc)
}  

csv_loc <- make_temp("csv_loc")
mzml_loc <- make_temp("mzml_loc")
sql_loc <- make_temp("sql_loc")



#Make some synthetic spectra data and write to csv:

random_spec <- lapply(1:10,
                      function(x){
                        rs <- list(sample(2000:30000, 20, replace = F))
                        IDBacApp::createFuzzyVector(massStart = 2000, 
                                                    massEnd = 30000,
                                                    ppm = 3000,
                                                    massList = rs,
                                                    intensityList = list(rep(100,
                                                                             length(rs))
                                                    )
                        )
                      })

for (i in seq_along(random_spec)) {
  write.table(cbind(4000:9091,
                    random_spec[[i]][,1]),  
              file = file.path(csv_loc,
                               paste0(LETTERS[[i]], ".csv")),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",")
}




nams <-  list.files(csv_loc, full.names = T)

pat <- make_temp("results")


z <- parseDelimitedMS(proteinPaths = nams,
                      proteinNames = tools::file_path_sans_ext(basename(nams)),
                      smallMolPaths = NULL,
                      smallMolNames = NULL,
                      exportDirectory = pat,
                      centroid = F)


test_that("parseDelimitedMS output", {
  expect_equal(z$mzFilePaths,
               paste0(pat, "/", LETTERS[1:10], ".mzML"))
  expect_equal(z$sampleIds,
               LETTERS[1:10])
})



zz <- lapply(z$mzFilePaths, 
             function(x){
               # [-13] removes line that has file path
               readLines(x)[-13]
             })

test_that("parseDelimitedMS conversion", {

  expect_known_hash(zz, "fdd36964b1")  

})
