
set.seed(42)
tempy <- tempdir()

dir.create(file.path(tempy,
                     "idbac_tests"))

sql_dir <- file.path(tempy,
                     "idbac_tests",
                     "sqlite")
dir.create(sql_dir)



protein_masses <- list(26978,
                       c(5112, 6625),
                       c(5873, 6261, 6584, 6908, 7108, 7512, 7877, 7982, 8538, 8609, 14085, 16087, 19514, 23704, 24162, 24295, 24978, 27095, 28946, 29040),
                       c(3091, 3951, 4070, 4772, 4821, 4846, 5090, 6488, 6493, 6675, 6962, 7187, 7252, 7655, 7672, 8034, 8913, 9030, 9469, 10030, 10071,
                         10514, 10573, 10599, 10867, 10958, 11165, 11476, 11705, 11910, 11948, 12413, 12984, 13178, 13245, 13368, 13455, 14019, 14081, 
                         14272, 14322, 14467, 14730, 14812, 15034, 15270, 15490, 15615, 16078, 16205, 16247, 16298, 16814, 17062, 17668, 17790, 17974,
                         18325, 18326, 18984, 19176, 19621, 19691, 20310, 20443, 20717, 20808, 20869, 21127, 21412, 22320, 22540, 22718, 23004, 23017, 
                         23943, 24387, 24459, 24732, 24869, 25352, 25387, 25742, 26525, 26926, 27005, 27059, 27529, 28133, 28342, 28657, 28735, 28854, 
                         28913, 29333, 29342, 29368, 29374, 29418, 29484))



intensities <- lapply(protein_masses, function(x) rep(100, length(x)))

protein_spectra <- createFuzzyVector(massStart = 2000, 
                                     massEnd = 30000,
                                     ppm = 8000, 
                                     massList = protein_masses, 
                                     intensityList = intensities)


small_masses <- list(1208,
                     c(1214, 2614),
                     c(225, 419, 461, 531, 546, 679, 822, 1076, 1484, 1709, 1775, 1890, 1893, 2138, 2259, 2414, 2442, 2624, 2724, 2971),
                     c(244, 297, 302, 308, 315, 334, 343, 346, 389, 401, 427, 468, 551, 565, 610, 675, 696, 728, 738, 760, 791, 840, 841,
                       906, 961, 962, 987, 989, 990, 998, 1018, 1033, 1049, 1060, 1123, 1154, 1156, 1165, 1210, 1240, 1243, 1280, 1297, 
                       1298, 1299, 1311, 1455, 1487, 1513, 1518, 1537, 1543, 1587, 1592, 1623, 1636, 1659, 1701, 1720, 1737, 1783, 1820, 
                       1821, 1825, 1860, 1899, 1978, 2011, 2080, 2114, 2134, 2137, 2189, 2212, 2265, 2325, 2339, 2345, 2389, 2394, 2417, 
                       2436, 2441, 2466, 2502, 2509, 2552, 2567, 2617, 2667, 2731, 2745, 2754, 2796, 2827, 2833, 2887, 2929, 2934, 2978))



intensities <- lapply(small_masses, function(x) rep(100, length(x)))

smallmol_spectra <- createFuzzyVector(massStart = 200, 
                                      massEnd = 3000,
                                      ppm = 100, 
                                      massList = small_masses, 
                                      intensityList = intensities)


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
  names(protein_spectra) <- LETTERS[1:4]
  names(smallmol_spectra) <- LETTERS[1:4]
  spectra <- c(smallmol_spectra, protein_spectra)
  spectra <- split(spectra, names(spectra))
})




test_that("created expected demo spectra", {
  
  expect_true(all(sapply(protein_spectra, function(x) inherits(x, "MassSpectrum"))))
  expect_true(all(lengths(lapply(protein_spectra, function(x) x@mass)) == 1771))
  expect_true(all(lengths(lapply(protein_spectra, function(x) x@intensity)) == 1771))
  
  expect_true(all(sapply(smallmol_spectra, function(x) inherits(x, "MassSpectrum"))))
  expect_true(all(lengths(lapply(smallmol_spectra, function(x) x@mass)) == 140021))
  expect_true(all(lengths(lapply(smallmol_spectra, function(x) x@intensity)) == 140021))
  
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
                 acquisitionInfo = NULL,
                 halfWindowSize = 2)
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
                                                proteinNames = LETTERS[1:4],
                                                smallMolPaths = s_files,
                                                smallMolNames = LETTERS[1:4],
                                                exportDirectory = new_mzml,
                                                centroid = F)

test_that("delim_to_mzml()", {
  
  expect_identical(length(delim_tomzml_results), 
                   4L)
  expect_identical(names(delim_tomzml_results),
                   LETTERS[1:4])
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
                           acquisitionInfo = NULL,
                           halfWindowSize = 2)
  )
})


samps <- idbac_available_samples(pool = new_idbac, 
                                 type = "all")
smallspec <- idbac_get_spectra(pool = new_idbac, 
                               sampleID = LETTERS[1:4],
                               type = "small")
protspec <- idbac_get_spectra(pool = new_idbac, 
                              sampleID = LETTERS[1:4],
                              type = "protein")


test_that("IDBac sql from csv is correct", {
  expect_identical(length(smallspec), 4L)
  expect_identical(length(protspec), 4L)
  expect_true(all(sapply(protspec, function(x) inherits(x, "MassSpectrum"))))
  expect_known_hash(smallspec, "78f3c87c6b")
  expect_known_hash(protspec, "802b0e3cf5")
})


new_idbac_spectra <- DBI::dbGetQuery(new_idbac, "SELECT * FROM spectra")
new_idbac_mzml_spectra <- DBI::dbGetQuery(new_idbac_mzml, "SELECT * FROM spectra")


test_that("compare the two created db spectra table", {
  # Don't include xml hash  because it includes file path
  expect_true(all(mapply(identical, new_idbac_spectra[4,-3], new_idbac_mzml_spectra[4,-3])))
})

