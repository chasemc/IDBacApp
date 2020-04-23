#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param minSNR minimum SNR a a peak must have to be retained
#' @param upperMassCutoff masses above this will be removed from analyses
#' @param pool sqlite pool
#' @param sampleIDs sample IDs of samples to process
#' @param mergeReplicates should replicates be merged? TRUE/FALSE
#' @param verbose should minfreq/minnum warning be displayed
#' 
#' @inheritParams .retrieve_peaks_from_pool
#' @inheritParams MALDIquant::filterPeaks
#' @inheritParams MALDIquant::binPeaks
#' @importFrom MALDIquant filterPeaks binPeaks mergeMassPeaks trim
#' @export
#' 
#' @return a single trimmed and binned MALDIquant peak object


idbac_get_peaks <- function(pool,
                            sampleIDs,
                            minFrequency = 0,
                            minNumber = NA, 
                            lowerMassCutoff,
                            upperMassCutoff, 
                            minSNR, 
                            tolerance = 0.002,
                            type,
                            mergeReplicates = TRUE,
                            method = "strict",
                            verbose = FALSE){
  
  if (!inherits(pool, "Pool")) {
    stop("pool not pool")
  }
  if (!inherits(minFrequency,  c("numeric", "integer"))) {
    stop("minFrequency not numeric")
  }
  if (!inherits(minNumber,  c("numeric","integer"))) {
    if (!is.na(minNumber)) {
    stop("minNumber not numeric or NA")
    }
  }
  if (!inherits(lowerMassCutoff, c("numeric", "integer"))) {
    stop("lowerMassCutoff not numeric")
  }
  if (!inherits(upperMassCutoff, c("numeric", "integer"))) {
    stop("upperMassCutoff not numeric")
  }
  if (!inherits(minSNR, c("integer", "numeric"))) {
    stop("minSNR not numeric")
  }  
  if (!inherits(tolerance, "numeric")) {
    stop("tolerance not numeric")
  }  
  if (!inherits(method, "character")) {
    stop("method not character")
  }
  if (!inherits(mergeReplicates, "logical")) {
    stop("mergeReplicates not type logical")
  }
  if (!type %in% c("protein",  "small", "all")) {
    stop('type must be one of c("protein",  "small", "all")')
  }
  
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
      temp <- binPeaks(temp,
                       tolerance = tolerance, 
                       method = method) 
      if (verbose){
      temp <- filterPeaks(temp,
                          minFrequency = minFrequency / 100,
                          minNumber = minNumber) 
      } else {
        suppressWarnings(
          temp <- filterPeaks(temp,
                              minFrequency = minFrequency / 100,
                              minNumber = minNumber)
        )
      }
      
      temp <- mergeMassPeaks(temp, 
                             method = "mean") 
      temp <- trim(temp,
                   c(lowerMassCutoff,
                     upperMassCutoff))
    } else {
      
      temp <- trim(temp,
                   c(lowerMassCutoff,
                     upperMassCutoff))
    }
    
    return(temp)
  })
  
}

