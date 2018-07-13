# -----------------
spectraProcessingFunction <- function(rawDataFilePaths,idbacDirectory){

  strReverse <- function(x) {
    sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
  }

  # Find sample name (fileName)
  sampleName <- tools::file_path_sans_ext(basename(rawDataFilePaths))

  # This code reads in mzXML files much faster than the MALDIquant functions, requires mzR

  spectraImport <- mzR::openMSfile(file = rawDataFilePaths)

  # First, check if there are more than one spectra stored in the mzXML file
  # If there are, make sure to de-nest and create separate MALDiquant objects
    spectraImport <- mzR::peaks(spectraImport)
    spectraImport <- lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[, 1],
                                                                                                intensity = x[, 2],
                                                                                                metaData = list(File = rawDataFilePaths,
                                                                                                                Strain = sampleName)))

  # Separate protein and small molecule spectra
  #Find protein set
  separateSpectra <- unlist(lapply(spectraImport, function(x)max(MALDIquant::mass(x))))
  proteinSpectra <- spectraImport[which(separateSpectra > 10000)]
  smallSpectra <- spectraImport[which(!separateSpectra > 10000)]
  # Cleanup
  remove(separateSpectra, spectraImport)

  # Average and save Protein Spectra  as RDS (Used to display a single spectra per sample in the protein spectra comparison plots)
  # Also, process spectra and peak pick individually and save as RDS
  if(length(proteinSpectra) > 0){
    averaged <- MALDIquant::averageMassSpectra(proteinSpectra, method = "mean")
    saveRDS(averaged, file.path(idbacDirectory, "Peak_Lists", paste0(averaged@metaData$Strain[[1]], "_", "SummedProteinSpectra.rds")))
    remove(averaged)
    gc()
    # Why square root transformation and not log:
    #  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
    # Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
    #"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
    #because for all three intensity groups in Table 1, the variance is approximately constant."

    proteinSpectra <- MALDIquant::transformIntensity(proteinSpectra, method = "sqrt")
    proteinSpectra <- MALDIquant::smoothIntensity(proteinSpectra, method = "SavitzkyGolay", halfWindowSize = 20)
    proteinSpectra <- MALDIquant::removeBaseline(proteinSpectra, method = "TopHat")
    proteinSpectra <- MALDIquant::detectPeaks(proteinSpectra, method = "MAD", halfWindowSize = 20, SNR = 4)
    saveRDS(proteinSpectra, file.path(idbacDirectory, "Peak_Lists", paste0(sampleName, "_", "ProteinPeaks.rds")))
    # Average and save Small Molecule Spectra as RDS (Used to display a single spectra per sample in the protein spectra comparison plots)
    # Also, process spectra and peak pick individually and save as RDS
  }
  if(length(smallSpectra) > 0){
    ############
    #Spectra Preprocessing, Peak Picking
    averaged <- MALDIquant::averageMassSpectra(smallSpectra, method = "mean")
    saveRDS(averaged, file.path(idbacDirectory,"Peak_Lists", paste0(averaged@metaData$Strain[[1]], "_", "SummedSmallMoleculeSpectra.rds")))
    remove(averaged)
    gc()
    smallSpectra <- MALDIquant::smoothIntensity(smallSpectra, method = "SavitzkyGolay", halfWindowSize = 20)
    smallSpectra <- MALDIquant::removeBaseline(smallSpectra, method = "TopHat")
    #Find all peaks with SNR >1, this will allow us to filter by SNR later, doesn't effect the peak-picking algorithm, just makes files bigger
    smallSpectra <- MALDIquant::detectPeaks(smallSpectra, method = "SuperSmoother", halfWindowSize = 20, SNR = 1)
    saveRDS(smallSpectra, file.path(idbacDirectory, "Peak_Lists", paste0(sampleName, "_", "SmallMoleculePeaks.rds")))

  }

}
