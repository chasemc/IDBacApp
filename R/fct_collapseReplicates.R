#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param minSNR minimum SNR a a peak must have to be retained
#' @param upperMassCutoff masses above this will be removed from analyses
#' @param pool sqlite pool
#' @param sampleIDs sample IDs of samples to process
#' @param peakPercentPresence peaks in replciates that occurr less frequently than this will be removed
#' @param tolerance binning tolerance ppm / 10e6
#' @param protein whether to search SQL for protein or small mol spectra

#'
#' 
#' @return a single trimmed and binned MALDIquant peak object


collapseReplicates <- function(pool,
                               sampleIDs,
                               peakPercentPresence,
                               lowerMassCutoff,
                               upperMassCutoff, 
                               minSNR, 
                               tolerance = 0.002,
                               protein){
  
  validate(need(is.numeric(peakPercentPresence), "peakPercentPresence not numeric"))
  validate(need(is.numeric(lowerMassCutoff), "lowerMassCutoff not numeric"))
  validate(need(is.numeric(upperMassCutoff), "upperMassCutoff not numeric"))
  validate(need(is.numeric(minSNR), "minSNR not numeric"))
  validate(need(is.numeric(tolerance), "tolerance not numeric"))
  validate(need(is.logical(protein), "protein not logical"))
  validate(need(!is.null(sampleIDs), "sampleIDs must not be NULL"))
  
  
  temp <- idbac_get_peaks(pool = pool,
                                    sampleIDs = sampleIDs,
                                    protein = protein)
  req(length(temp) > 0)
  # Binning peaks lists belonging to a single sample so we can filter 
  # peaks outside the given threshold of presence 
  
  lapply(temp, function(temp){
    
    for (i in 1:length(temp)) {
      snr1 <-  which(MALDIquant::snr(temp[[i]]) >= minSNR)
      temp[[i]]@mass <- temp[[i]]@mass[snr1]
      temp[[i]]@snr <- temp[[i]]@snr[snr1]
      temp[[i]]@intensity <- temp[[i]]@intensity[snr1]
    }
    
    specNotZero <- sapply(temp, function(x) length(x@mass) > 0)
    
    # Only binPeaks if spectra(um) has peaks.
    # see: https://github.com/sgibb/MALDIquant/issues/61 for more info 
    # note: MALDIquant::binPeaks does work if there is only one spectrum
    if (any(specNotZero)) {
      
      temp <- temp[specNotZero]
      temp <- MALDIquant::binPeaks(temp,
                                   tolerance = tolerance, 
                                   method = c("strict")) 
      
      temp <- MALDIquant::filterPeaks(temp,
                                      minFrequency = peakPercentPresence / 100) 
      
      temp <- MALDIquant::mergeMassPeaks(temp, 
                                         method = "mean") 
      temp <- MALDIquant::trim(temp,
                               c(lowerMassCutoff,
                                 upperMassCutoff))
    } else {
      temp <- MALDIquant::mergeMassPeaks(temp, 
                                         method = "mean") 
    }
    
    
    return(temp)
  })
  
}


