
set.seed(42)

masses <- mapply(
  function(x, y){
    sort(sample(seq(3000, 30000, by = .1),
                size = y, 
                replace = FALSE)
    )},
  1:104,
  sample(1:100, 104,replace = T),
  SIMPLIFY = F)


intensities <- lapply(masses, function(x) rep(100, length(x)))

protein_spectra <- createFuzzyVector(massStart = 2000, 
                                     massEnd = 30000,
                                     ppm = 8000, 
                                     massList = masses, 
                                     intensityList = intensities)


masses <- mapply(function(x, y){
  sort(sample(seq(200, 3000, by = .1),
              size = y, 
              replace = FALSE)
  )
},
1:104,
sample(1:100, 104,replace = T),
SIMPLIFY = F)

intensities <- lapply(masses, function(x) rep(100, length(x)))

smallmol_spectra <- createFuzzyVector(massStart = 200, 
                                      massEnd = 3000,
                                      ppm = 20000, 
                                      massList = masses, 
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


labels(protein_spectra) <- LETTERS
labels(smallmol_spectra) <- LETTERS
suppressWarnings({
  spectra <- c(smallmol_spectra, protein_spectra)
  spectra <- split(spectra, labels(spectra))
})
remove("smallmol_spectra", "protein_spectra")




test_that("created expected demo spectra", {
  testthat::expect_known_hash(spectra,
                              "f04f75d8b0")
})

# Save as mzml ------------------------------------------------------------

a <- tempdir()
dir.create(file.path(a,
                     "idbac_tests"))
mzml_path <- file.path(a,
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

test_that("wrote demo mzml", {
  expect_equal(list.files(mzml_path), 
               c("A.mzml", 
                 "B.mzml", 
                 "C.mzml", 
                 "D.mzml", 
                 "E.mzml", 
                 "F.mzml", 
                 "G.mzml", 
                 "H.mzml", 
                 "I.mzml", 
                 "J.mzml", 
                 "K.mzml", 
                 "L.mzml", 
                 "M.mzml", 
                 "N.mzml", 
                 "O.mzml", 
                 "P.mzml", 
                 "Q.mzml", 
                 "R.mzml", 
                 "S.mzml", 
                 "T.mzml", 
                 "U.mzml", 
                 "V.mzml", 
                 "W.mzml", 
                 "X.mzml", 
                 "Y.mzml", 
                 "Z.mzml")
  )
})
