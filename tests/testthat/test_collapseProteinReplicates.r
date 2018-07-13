context("collapseProteinReplicates")




fakeSpectrum1 <- MALDIquant::createMassPeaks(mass = seq(from = 10, to = 100, by = 10) ,
                                           intensity = rep(100, 10),
                                           metaData = list(Strain = "A"))


fakeSpectrum2 <- MALDIquant::createMassPeaks(mass = seq(from = 10, to = 50, by = 10) ,
                                             intensity = rep(50, 5),
                                             metaData = list(Strain = "A"))


fakeSpectra <- c(fakeSpectrum1, fakeSpectrum2)


prot50 <- collapseProteinReplicates(fakeSpectra, proteinPercentPresence = 50)
prot100 <- collapseProteinReplicates(fakeSpectra, proteinPercentPresence = 100)





test_that("Protein replicates are collapsed into single peak list", {
  expect_equal(length(prot50), 1)
  expect_equal(length(prot100), 1)
  expect_equal(prot50[[1]]@mass, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))
  expect_equal(prot50[[1]]@intensity, c(150, 150, 150, 150, 150, 100, 100, 100, 100, 100))
  expect_equal(prot100[[1]]@mass, c(10, 20, 30, 40, 50))
  expect_equal(prot100[[1]]@intensity, c(150, 150, 150, 150, 150))
})



