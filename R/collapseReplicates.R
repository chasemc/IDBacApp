#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param fileshas the shas for the individual MALDIquant peak objects
#' @param proteinPercentPresence value between 0 and 1 (percent presence translated to 0-100%)
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param checkedOutPool database pool connection
#' @param minSNR minimum snr a peak must have to be retained
#' @param upperMassCutoff masses above this will be removed from analyses
#'
#' @export
#' @return a single trimmed and binned MALDIquant peak object


collapseProteinReplicates <- function(checkedOutPool,
                                      fileshas,
                                      proteinPercentPresence,
                                      lowerMassCutoff,
                                      upperMassCutoff, 
                                      minSNR){
  
  temp <- IDBacApp::getProteinPeakData(checkedOutPool = checkedOutPool,
                                       fileshas = fileshas) 
  # Binning peaks lists belonging to a single sample so we can filter 
  # peaks outside the given threshold of presence 
  
  for(i in 1:length(temp)){
    snr1 <-  which(MALDIquant::snr(temp[[i]]) >= minSNR)
    temp[[i]]@mass <- temp[[i]]@mass[snr1]
    temp[[i]]@snr <- temp[[i]]@snr[snr1]
    temp[[i]]@intensity <- temp[[i]]@intensity[snr1]
  }
  
  
  
  temp <- MALDIquant::binPeaks(temp,
                               tolerance = .02, 
                               method = c("strict"))
  temp <- MALDIquant::filterPeaks(temp,
                                  minFrequency = proteinPercentPresence / 100)
  temp <- MALDIquant::mergeMassPeaks(temp, 
                                     method = "mean")
  return(MALDIquant::trim(temp,
                          c(lowerMassCutoff,
                            upperMassCutoff)))
}


#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param checkedOutPool pool to database
#' @param fileshas the shas for the individual MALDIquant peak objects
#'
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export


getProteinPeakData <-  function(checkedOutPool, fileshas){
  
  sqlQ <- glue::glue_sql("
                         SELECT `proteinPeaks`
                         FROM (SELECT *
                         FROM `IndividualSpectra`
                         WHERE (`spectrumSHA` IN ({shas*})))
                         WHERE (`proteinPeaks` IS NOT NULL)",
                         shas = fileshas,
                         .con = checkedOutPool
  )
  
  results <- DBI::dbGetQuery(checkedOutPool, sqlQ)
  results <- unname(unlist(results, recursive = FALSE))
  unlist(lapply(results, function(x) unserialize(memDecompress(x, type = "gzip")))  )
  
}











#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param fileshas the shas for the individual MALDIquant peak objects
#' @param proteinPercentPresence value between 0 and 1 (percent presence translated to 0-100%)
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param checkedOutPool pool connection to database
#' @param minSNR minimum SNR a a peak must have to be retained
#' @param upperMassCutoff masses above this will be removed from analyses
#'
#' @export
#' @return a single trimmed and binned MALDIquant peak object


collapseSmallMolReplicates <- function(checkedPool,
                                       sampleIDs,
                                       peakPercentPresence,
                                       lowerMassCutoff,
                                       upperMassCutoff, 
                                       minSNR){
  
  temp <- IDBacApp::getSmallMolPeakData(checkedPool = checkedPool,
                                        sampleIDs = sampleIDs) 
  # Binning peaks lists belonging to a single sample so we can filter 
  # peaks outside the given threshold of presence 
  
  for(i in 1:length(temp)){
    snr1 <-  which(MALDIquant::snr(temp[[i]]) >= minSNR)
    temp[[i]]@mass <- temp[[i]]@mass[snr1]
    temp[[i]]@snr <- temp[[i]]@snr[snr1]
    temp[[i]]@intensity <- temp[[i]]@intensity[snr1]
  }
  
  temp <- MALDIquant::binPeaks(temp,
                               tolerance = 0.02, 
                               method = c("strict")) 
  
  temp <- MALDIquant::filterPeaks(temp,
                                  minFrequency = peakPercentPresence / 100) 
  
  temp <- MALDIquant::mergeMassPeaks(temp, 
                                     method = "mean") 
  
  return(MALDIquant::trim(temp,
                          c(lowerMassCutoff,
                            upperMassCutoff)))
}


#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param checkedOutPool database pool connection
#' @param sampleIDs the shas for the individual MALDIquant peak objects
#'
#' @return unlisted MALDIquant peak objects correspoding to the provided sampleIDs
#' @export


getSmallMolPeakData <-  function(checkedPool, sampleIDs){
  
  
  query <- DBI::dbSendStatement("SELECT `smallMoleculePeaks`
                                FROM IndividualSpectra
                                WHERE (`smallMoleculePeaks` IS NOT NULL)
                                AND (`Strain_ID` = ?)",
                                con=con)
  

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

