#' Assemble mirror plot Data
#'
#' @param sampleID1 sample ID to search in IDBac sqlite database (will be positive spectrum)
#' @param sampleID2 sample ID to search in IDBac sqlite database (will be negative spectrum)
#' @param peakPercentPresence numeric between 0 and 100, peakPercentPresence
#' @param lowerMassCutoff lowerMassCutoff
#' @param upperMassCutoff upperMassCutoff
#' @param minSNR numeric, peaks with a SNR below this number will be removed
#' @param tolerance MALDIquant binning tolerance for intra-sample binning
#' @param pool1 pool that contains sample 1 (positive spectrum)
#' @param pool2 pool that contains sample 2 (negative spectrum)
#' @param normalizeSpectra should spectra be normalized? TRUE/FALSE
#'
#' @return environment containing mirror plot data
#' 
#'
assembleMirrorPlots <- function(sampleID1,
                                sampleID2,
                                peakPercentPresence,
                                lowerMassCutoff,
                                upperMassCutoff,
                                minSNR,
                                tolerance = 0.002,
                                pool1,
                                pool2,
                                normalizeSpectra = FALSE){
  
  mirrorPlotEnv <- new.env(parent = parent.frame())
  
  
  # get protein peak data for the 1st mirror plot selection
  

  # need(checkSinglePool(pool1))
  # need(checkSinglePool(pool2))
  # need(!is.null(sampleID1))
  # need(!is.null(sampleID2))
  
  mirrorPlotEnv$sampleIDOne <- sampleID1
  mirrorPlotEnv$sampleIDTwo <- sampleID2
  
  
  mirrorPlotEnv$peaksSampleOne <- collapseReplicates(pool = pool1,
                                                               sampleIDs = sampleID1,
                                                               peakPercentPresence = peakPercentPresence,
                                                               lowerMassCutoff = lowerMassCutoff,
                                                               upperMassCutoff = upperMassCutoff,
                                                               minSNR = minSNR,
                                                               tolerance = tolerance,
                                                               protein = TRUE)[[1]] 
  
  
  mirrorPlotEnv$peaksSampleTwo <- collapseReplicates(pool = pool2,
                                                               sampleIDs = sampleID2,
                                                               peakPercentPresence = peakPercentPresence,
                                                               lowerMassCutoff = lowerMassCutoff,
                                                               upperMassCutoff = upperMassCutoff,
                                                               minSNR = minSNR,
                                                               tolerance = tolerance,
                                                               protein = TRUE)[[1]]
 
  
  shiny::validate(
    shiny::need(sum(length(mirrorPlotEnv$peaksSampleOne@mass),
                    length(mirrorPlotEnv$peaksSampleTwo@mass)) > 0,
                "No peaks found in either sample, double-check the settings or your raw data.")
  )
  # Binpeaks for the two samples so we can color code similar peaks within the plot
  # Note: different binning algorithm than used for hierarchical clustering
  temp <- MALDIquant::binPeaks(c(mirrorPlotEnv$peaksSampleOne, mirrorPlotEnv$peaksSampleTwo), tolerance = tolerance)
  
  mirrorPlotEnv$peaksSampleOne <- temp[[1]]
  mirrorPlotEnv$peaksSampleTwo <- temp[[2]]
  
  # Set all peak colors for positive spectrum as red
  mirrorPlotEnv$SampleOneColors <- rep("red", length(mirrorPlotEnv$peaksSampleOne@mass))
  # Which peaks top samaple one are also in the bottom sample:
  temp <- mirrorPlotEnv$peaksSampleOne@mass %in% mirrorPlotEnv$peaksSampleTwo@mass
  # Color matching peaks in positive spectrum blue
  mirrorPlotEnv$SampleOneColors[temp] <- "blue"
  remove(temp)
  
  conn <- pool::poolCheckout(pool1)
  
  
  mirrorPlotEnv$spectrumSampleOne <- MALDIquant::averageMassSpectra(idbac_get_spectra(pool = pool1,
                                                                                                sampleID = sampleID1, 
                                                                                                protein = TRUE,
                                                                                                smallmol = FALSE))
  
  
  if (normalizeSpectra) {
    mirrorPlotEnv$spectrumSampleOne <- normalizeSpectrumIntensity(mirrorPlotEnv$spectrumSampleOne)

  }
  
  
  pool::poolReturn(conn)
  conn <- pool::poolCheckout(pool2)
  
  
  mirrorPlotEnv$spectrumSampleTwo <- MALDIquant::averageMassSpectra(idbac_get_spectra(pool = pool2,
                                                                                                sampleID = sampleID2, 
                                                                                                protein = TRUE,
                                                                                                smallmol = FALSE))

  
  
  if (normalizeSpectra) {
    mirrorPlotEnv$spectrumSampleTwo <- normalizeSpectrumIntensity(mirrorPlotEnv$spectrumSampleTwo)
    

  }
  
  pool::poolReturn(conn)
  
  
  return(mirrorPlotEnv)
}
