
#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param checkedPool checked out pool
#' @param sampleIDs sample IDs of samples to process
#' @param protein whether to search SQL for protein or small mol spectra
#'
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export


getPeakData <-  function(checkedPool, sampleIDs, protein){
  
  if (!is.logical(protein)) {stop("In getPeakData, provided value for 'protein' wasn't logical-type.")}
  
  if (protein == TRUE) {
    sym <- '>'
  } else {
    sym <- '<'
  }  
    
    query <- DBI::dbSendStatement(glue::glue("SELECT peakMatrix
                              FROM IndividualSpectra
                                  WHERE maxMass {sym} 6000
                                  AND (Strain_ID = ?)"),
                                  con = checkedPool)
    
    DBI::dbBind(query, list(as.character(as.vector(sampleIDs))))
    results <- DBI::dbFetch(query)
    DBI::dbClearResult(query)
    
    results <- lapply(results[,1], jsonlite::fromJSON)
    
    results <- lapply(results,
                function(x){
                  MALDIquant::createMassPeaks(mass = x$mass,
                                              intensity = x$intensity ,
                                              snr = as.numeric(x$snr))
                }
    )
   
    
  }
  




#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param minSNR minimum SNR a a peak must have to be retained
#' @param upperMassCutoff masses above this will be removed from analyses
#' @param checkedPool checked-out pool
#' @param sampleIDs sample IDs of samples to process
#' @param peakPercentPresence peaks in replciates that occurr less frequently than this will be removed
#' @param tolerance binning tolerance ppm / 10e6
#' @param protein whether to search SQL for protein or small mol spectra

#'
#' @export
#' @return a single trimmed and binned MALDIquant peak object


collapseReplicates <- function(checkedPool,
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
  
  
  
  temp <- IDBacApp::getPeakData(checkedPool = checkedPool,
                                sampleIDs = sampleIDs,
                                protein = protein) 
  req(length(temp) > 0)
  # Binning peaks lists belonging to a single sample so we can filter 
  # peaks outside the given threshold of presence 
  
  for (i in 1:length(temp)) {
    snr1 <-  which(MALDIquant::snr(temp[[i]]) >= minSNR)
    temp[[i]]@mass <- temp[[i]]@mass[snr1]
    temp[[i]]@snr <- temp[[i]]@snr[snr1]
    temp[[i]]@intensity <- temp[[i]]@intensity[snr1]
  }
  
  specNotZero <- sapply(temp, function(x) length(x@mass) > 0)
  
  
  if (any(specNotZero)) {
  
  temp <- temp[specNotZero]
  temp <- MALDIquant::binPeaks(temp,
                               tolerance = tolerance, 
                               method = c("strict")) 
  
  temp <- MALDIquant::filterPeaks(temp,
                                  minFrequency = peakPercentPresence / 100) 
  
  temp <- MALDIquant::mergeMassPeaks(temp, 
                                     method = "mean") 
  } else {
    # Intentionally blank
  }
  
  
  return(MALDIquant::trim(temp,
                          c(lowerMassCutoff,
                            upperMassCutoff)))
}


