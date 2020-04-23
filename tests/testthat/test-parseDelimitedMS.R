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
                        rs <- list(c(9560,27783,6938,11605,28398,18580,11869,19599,17081,22014,3216,23928,12121,19456,24987,7141,22224,23880,10323,11794))
                        createFuzzyVector(massStart = 2000, 
                                          massEnd = 30000,
                                          ppm = 3000,
                                          massList = rs,
                                          intensityList = list(rep(100,
                                                                   length(rs))
                                          )
                        )
                      })

for (i in seq_along(random_spec)) {
  write.table(cbind(4000:8687,
                    random_spec[[i]][,1]),  
              file = file.path(csv_loc,
                               paste0(LETTERS[[i]], ".csv")),
              row.names = FALSE,
              col.names = FALSE,
              sep = ",")
}




nams <-  list.files(csv_loc, full.names = T)

pat <- make_temp("results")

z <- delim_to_mzml(proteinPaths = nams,
                   proteinNames = tools::file_path_sans_ext(basename(nams)),
                   smallMolPaths = NULL,
                   smallMolNames = NULL,
                   exportDirectory = pat,
                   centroid = F)


test_that("delim_to_mzml output", {
  expect_true(all(file.exists(z)))
  expect_equal(length(z), 10)
  expect_equal(names(z),
               LETTERS[1:10])
})



zz <- lapply(z, 
             function(x){
               # [-13] removes line that has file path
               readLines(x)[-13]
             })


# read mzml back in
a <- lapply(z, mzR::openMSfile)
a <- lapply(a, mzR::peaks)

test_that("parseDelimitedMS conversion retuens correct spectra", {
  expect_known_hash(a, "8bc06a3792")  
  expect_equal(a[[1]][, 2],
               as.numeric(random_spec[[1]][, 1]))
  expect_equal(a[[4]][, 2],
               as.numeric(random_spec[[4]][, 1]))
})
