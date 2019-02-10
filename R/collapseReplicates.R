
#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param checkedPool checked out pool
#' @param sampleIDs sample IDs of samples to process
#' @param protein whether to search SQL for protein or small mol spectra
#'
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export


getPeakData <-  function(checkedPool, sampleIDs, protein){
  
  if(protein == TRUE){
    query <- DBI::dbSendStatement("SELECT `proteinPeaks`
                                FROM IndividualSpectra
                                WHERE (`proteinPeaks` IS NOT NULL)
                                AND (`Strain_ID` = ?)",
                                  con = checkedPool)
    
    
    DBI::dbBind(query, list(as.character(as.vector(sampleIDs))))
    results <- DBI::dbFetch(query)
    DBI::dbClearResult(query)
    
    results <- unname(unlist(results, recursive = FALSE))
    unlist(lapply(results, 
                  function(x) 
                    unserialize(memDecompress(x, 
                                              type = "gzip")
                    )
    ) 
    )
  } else {
    
    query <- DBI::dbSendStatement("SELECT `smallMoleculePeaks`
                                FROM IndividualSpectra
                                  WHERE (`smallMoleculePeaks` IS NOT NULL)
                                  AND (`Strain_ID` = ?)",
                                  con = checkedPool)
    
    
    DBI::dbBind(query, list(as.character(as.vector(sampleIDs))))
    results <- DBI::dbFetch(query)
    DBI::dbClearResult(query)
    
    results <- unname(unlist(results, recursive = FALSE))
    unlist(lapply(results, 
                  function(x) 
                    unserialize(memDecompress(x, 
                                              type = "gzip")
                    )
    ) 
    )
  }
  
  
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
  

  
  temp <- IDBacApp::getPeakData(checkedPool = checkedPool,
                                sampleIDs = sampleIDs,
                                protein = protein) 
  # Binning peaks lists belonging to a single sample so we can filter 
  # peaks outside the given threshold of presence 
  
  for(i in 1:length(temp)){
    snr1 <-  which(MALDIquant::snr(temp[[i]]) >= minSNR)
    temp[[i]]@mass <- temp[[i]]@mass[snr1]
    temp[[i]]@snr <- temp[[i]]@snr[snr1]
    temp[[i]]@intensity <- temp[[i]]@intensity[snr1]
  }
  
  temp <- MALDIquant::binPeaks(temp,
                               tolerance = tolerance, 
                               method = c("strict")) 
  
  temp <- MALDIquant::filterPeaks(temp,
                                  minFrequency = peakPercentPresence / 100) 
  
  temp <- MALDIquant::mergeMassPeaks(temp, 
                                     method = "mean") 
  
  return(MALDIquant::trim(temp,
                          c(lowerMassCutoff,
                            upperMassCutoff)))
}


