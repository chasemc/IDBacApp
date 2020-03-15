
set.seed(42)
tempy <- tempdir()

dir.create(file.path(tempy,
                     "idbac_tests"))

sql_dir <- file.path(tempy,
                     "idbac_tests",
                     "sqlite")
dir.create(sql_dir)

protein_masses <- mapply(
  function(x, y){
    sort(sample(seq(3000, 30000, by = .1),
                size = y, 
                replace = FALSE)
    )},
  1:26,
  sample(1:100, 26,replace = T),
  SIMPLIFY = F)


intensities <- lapply(protein_masses, function(x) rep(100, length(x)))

protein_spectra <- createFuzzyVector(massStart = 2000, 
                                     massEnd = 30000,
                                     ppm = 8000, 
                                     massList = protein_masses, 
                                     intensityList = intensities)


small_masses <- mapply(function(x, y){
  sort(sample(seq(200, 3000, by = .1),
              size = y, 
              replace = FALSE)
  )
},
1:26,
sample(1:100, 26,replace = T),
SIMPLIFY = F)

intensities <- lapply(small_masses, function(x) rep(100, length(x)))

smallmol_spectra <- createFuzzyVector(massStart = 200, 
                                      massEnd = 3000,
                                      ppm = 100, 
                                      massList = small_masses, 
                                      intensityList = intensities)


# if you want to plot this:
# plot(as.numeric(rownames(protein_spectra)),
#      protein_spectra[,1], 
#      type = "o")


smallmol_spectra <- lapply(1:ncol(smallmol_spectra), 
                           function(x){
                             MALDIquant::createMassSpectrum(mass = as.numeric(rownames(smallmol_spectra)),
                                                            intensity = smallmol_spectra[, x])
                           })

protein_spectra <- lapply(1:ncol(protein_spectra), 
                          function(x){
                            MALDIquant::createMassSpectrum(mass = as.numeric(rownames(protein_spectra)),
                                                           intensity = protein_spectra[, x])
                          })

suppressWarnings({
  labels(protein_spectra) <- LETTERS
  labels(smallmol_spectra) <- LETTERS
  spectra <- c(smallmol_spectra, protein_spectra)
  spectra <- split(spectra, labels(spectra))
})




test_that("created expected demo spectra", {
  testthat::expect_known_hash(spectra,
                              "b9b4ed3e30")
})

# Save as mzml ------------------------------------------------------------

mzml_path <- file.path(tempy,
                       "idbac_tests",
                       "mzml", fsep = "/")
dir.create(mzml_path)
mzml_path <- normalizePath(mzml_path,
                           "/")

for (i in seq_along(spectra)) {
  
  MALDIquantForeign::exportMzMl(unlist(spectra[[i]]),
                                file.path(mzml_path,
                                          paste0(LETTERS[[i]],
                                                 ".mzml"))
  )
}




IDBacApp::idbac_create("sql_from_mzml",
                       sql_dir)

new_idbac_mzml <- IDBacApp::idbac_connect("sql_from_mzml",
                                          sql_dir)[[1]]

test_that("IDBac sql from mzml", {
  expect_message(
    db_from_mzml(mzFilePaths = list.files(mzml_path, full.names = T),
                 sampleIds = basename(tools::file_path_sans_ext(list.files(mzml_path, full.names = T))),
                 idbacPool = new_idbac_mzml,
                 acquisitionInfo = NULL)
  )
})




# Create txt --------------------------------------------------------------


a <- tempdir()
csv_path <- file.path(tempy,
                      "idbac_tests",
                      "csv", fsep = "/")
dir.create(csv_path)
csv_path <- normalizePath(csv_path,
                          "/")

dir.create(file.path(tempy,
                     "idbac_tests",
                     "csv",
                     "small",
                     fsep = "/"))
dir.create(file.path(tempy,
                     "idbac_tests",
                     "csv",
                     "protein",
                     fsep = "/"))

for (i in seq_along(smallmol_spectra)) {
  
  MALDIquantForeign::exportCsv(smallmol_spectra[[i]],
                               file.path(csv_path,
                                         "small",
                                         paste0(names(smallmol_spectra)[[i]],
                                                ".csv"))
  )
}

for (i in seq_along(protein_spectra)) {
  
  MALDIquantForeign::exportCsv(protein_spectra[[i]],
                               file.path(csv_path,
                                         "protein",
                                         paste0(names(protein_spectra)[[i]],
                                                ".csv"))
  )
}


p_files <- list.files(file.path(csv_path,
                                "protein"),
                      full.names = T)
s_files <- list.files(file.path(csv_path,
                                "small"),
                      full.names = T)
new_mzml <- file.path(csv_path,
                      "mzml")
dir.create(new_mzml)
delim_tomzml_results <- IDBacApp::delim_to_mzml(proteinPaths = p_files,
                                                proteinNames = LETTERS,
                                                smallMolPaths = s_files,
                                                smallMolNames = LETTERS,
                                                exportDirectory = new_mzml,
                                                centroid = F)

test_that("delim_to_mzml()", {
  
  expect_identical(length(delim_tomzml_results), 
                   26L)
  expect_identical(names(delim_tomzml_results),
                   LETTERS)
  expect_true(all(sapply(delim_tomzml_results, file.exists)))
  expect_equal(unique(tools::file_ext(delim_tomzml_results)), 
               "mzml")
})




IDBacApp::idbac_create("sql_from_csv",
                       sql_dir)

new_idbac <- IDBacApp::idbac_connect("sql_from_csv",
                                     sql_dir)[[1]]

test_that("IDBac sql from csv", {
  expect_message(
    IDBacApp::db_from_mzml(mzFilePaths = unname(delim_tomzml_results),
                           sampleIds = names(delim_tomzml_results),
                           idbacPool = new_idbac,
                           acquisitionInfo = NULL)
  )
})


samps <- idbac_available_samples(new_idbac, 
                                 allSamples = T)
smallspec <- idbac_get_spectra(pool = new_idbac, 
                               sampleID = LETTERS,
                               protein = F,
                               smallmol = T)
protspec <- idbac_get_spectra(pool = new_idbac, 
                              sampleID = LETTERS,
                              protein = T,
                              smallmol = F)

test_that("IDBac sql from csv is correct", {
  expect_identical(length(smallspec), 26L)
  expect_identical(length(protspec), 26L)
  expect_true(all(sapply(protspec, function(x) inherits(x, "MassSpectrum"))))
  expect_known_hash(smallspec, "3f3aa0d814")
  expect_known_hash(protspec, "13db374ea8")
})




dplyr::tbl(new_idbac, "spectra") %>% dplyr::collect() -> new_idbac_spectra
dplyr::tbl(new_idbac_mzml, "spectra") %>% dplyr::collect() -> new_idbac_mzml_spectra







mapply(identical, new_idbac_spectra[,-3], new_idbac_mzml_spectra[,-3])








