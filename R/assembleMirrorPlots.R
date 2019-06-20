#' Assemble mirror plot Data
#'
#' @param sampleID1 sample ID to search in IDBac sqlite database (will be positive spectrum)
#' @param sampleID2 sample ID to search in IDBac sqlite database (will be negative spectrum)
#' @param peakPercentPresence numeric between 0% and 100%, peakPercentPresence
#' @param lowerMassCutoff lowerMassCutoff
#' @param upperMassCutoff upperMassCutoff
#' @param minSNR numeric, peaks with a SNR below this number will be removed
#' @param pool IDBac database pool
#'
#' @return environment containing mirror plot data
#' @export
#'
assembleMirrorPlots <- function(sampleID1 = input$Spectra1,
                                sampleID2 = input$Spectra2,
                                peakPercentPresence = input$percentPresence,
                                lowerMassCutoff = input$lowerMass,
                                upperMassCutoff = input$upperMass,
                                minSNR = input$SNR,
                                pool = workingDB$pool()){
  
  mirrorPlotEnv <- new.env(parent = parent.frame())
  
  # connect to sql
  conn <- pool::poolCheckout(pool)
  
  # get protein peak data for the 1st mirror plot selection
  
  
  mirrorPlotEnv$peaksSampleOne <- IDBacApp::collapseReplicates(checkedPool = conn,
                                                               sampleIDs = sampleID1,
                                                               peakPercentPresence = peakPercentPresence,
                                                               lowerMassCutoff = lowerMassCutoff,
                                                               upperMassCutoff = upperMassCutoff,
                                                               minSNR = minSNR,
                                                               tolerance = 0.002,
                                                               protein = TRUE) 
  
  mirrorPlotEnv$peaksSampleTwo <- IDBacApp::collapseReplicates(checkedPool = conn,
                                                               sampleIDs = sampleID2,
                                                               peakPercentPresence = peakPercentPresence,
                                                               lowerMassCutoff = lowerMassCutoff,
                                                               upperMassCutoff = upperMassCutoff,
                                                               minSNR = minSNR,
                                                               tolerance = 0.002,
                                                               protein = TRUE)
  
  
  shiny::validate(
    shiny::need(sum(length(mirrorPlotEnv$peaksSampleOne@mass),
                    length(mirrorPlotEnv$peaksSampleTwo@mass)) > 0,
                "No peaks found in either sample, double-check the settings or your raw data.")
  )
  # Binpeaks for the two samples so we can color code similar peaks within the plot
  # Note: different binning algorithm than used for hierarchical clustering
  temp <- MALDIquant::binPeaks(c(mirrorPlotEnv$peaksSampleOne, mirrorPlotEnv$peaksSampleTwo), tolerance = .002)
  
  mirrorPlotEnv$peaksSampleOne <- temp[[1]]
  mirrorPlotEnv$peaksSampleTwo <- temp[[2]]
  
  # Set all peak colors for positive spectrum as red
  mirrorPlotEnv$SampleOneColors <- rep("red", length(mirrorPlotEnv$peaksSampleOne@mass))
  # Which peaks top samaple one are also in the bottom sample:
  temp <- mirrorPlotEnv$peaksSampleOne@mass %in% mirrorPlotEnv$peaksSampleTwo@mass
  # Color matching peaks in positive spectrum blue
  mirrorPlotEnv$SampleOneColors[temp] <- "blue"
  remove(temp)
  
  
  
  mirrorPlotEnv$spectrumSampleOne <- IDBacApp::mquantSpecFromSQL(checkedPool = conn,
                                                                 sampleID = sampleID1, 
                                                                 proteinOrSmall = '>')
  
  
  
  
  mirrorPlotEnv$spectrumSampleTwo <- IDBacApp::mquantSpecFromSQL(checkedPool = conn,
                                                                 sampleID = sampleID2, 
                                                                 proteinOrSmall = '>')
  
  pool::poolReturn(conn)
  
  return(mirrorPlotEnv)
}