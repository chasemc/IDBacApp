#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param minSNR minimum SNR a a peak must have to be retained
#' @param upperMassCutoff masses above this will be removed from analyses
#' @param pool sqlite pool
#' @param sampleIDs sample IDs of samples to process
#' @param peakPercentPresence peaks in replciates that occurr less frequently than this will be removed
#' @param tolerance binning tolerance ppm / 10e6
#' @param mergeReplicates should replicates be merged? TRUE/FALSE
#'
#' @inheritParams .retrieve_peaks_from_pool
#'
#' @export
#' 
#' @return a single trimmed and binned MALDIquant peak object


idbac_get_peaks <- function(pool,
                            sampleIDs,
                            peakPercentPresence,
                            lowerMassCutoff,
                            upperMassCutoff, 
                            minSNR, 
                            tolerance = 0.002,
                            type,
                            mergeReplicates = TRUE){
  
  validate(need(is.numeric(peakPercentPresence), "peakPercentPresence not numeric"))
  validate(need(is.numeric(lowerMassCutoff), "lowerMassCutoff not numeric"))
  validate(need(is.numeric(upperMassCutoff), "upperMassCutoff not numeric"))
  validate(need(is.numeric(minSNR), "minSNR not numeric"))
  validate(need(is.numeric(tolerance), "tolerance not numeric"))
  validate(need(is.logical(mergeReplicates), "mergeReplicates not logical"))
  .checkPool(pool)
  
  if (is.null(sampleIDs)) {
    sampleIDs <- IDBacApp::idbac_available_samples(pool = pool,
                                                   type = type)
    }
  
  
  temp <- .retrieve_peaks_from_pool(pool = pool,
                                    ids = sampleIDs,
                                    type = type,
                                    minSNR = minSNR)
  req(length(temp) > 0)
  # Binning peaks lists belonging to a single sample so we can filter 
  # peaks outside the given threshold of presence 
  
  lapply(temp, function(temp){
    
    
    specNotZero <- sapply(temp, function(x) length(x@mass) > 0)
    
    # Only binPeaks if spectra(um) has peaks.
    # see: https://github.com/sgibb/MALDIquant/issues/61 for more info 
    # note: MALDIquant::binPeaks does work if there is only one spectrum
    
    if (any(specNotZero) & mergeReplicates) {
      
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
      
      temp <- MALDIquant::trim(temp,
                               c(lowerMassCutoff,
                                 upperMassCutoff))
    }
    
    return(temp)
  })
  
}

