#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param db pool::() connection to the IDBac sqlite dataabse
#' @param fileshas the shas for the individual MALDIquant peak objects
#' @param proteinPercentPresence value between 0 and 1 (percent presence translated to 0-100%)
#' @param lowerMassCutoff masses below this will be removed from analyses
#' @param upperMassCutoff masses above this will be removed from analyses
#' @export
#' @return a single trimmed and binned MALDIquant peak object


collapseProteinReplicates <- function(db,
                                      fileshas,
                                      proteinPercentPresence,
                                      lowerMassCutoff,
                                      upperMassCutoff, 
                                      dbConnection){
  
  IDBacApp::getProteinPeakData(db =db,
                               dbConnection = dbConnection,
                               fileshas = fileshas) %>%
    # Binning peaks lists belonging to a single sample so we can filter 
    # peaks outside the given threshold of presence 
    MALDIquant::binPeaks(.,
                         tolerance = .02, method = c("strict")) %>%
    MALDIquant::filterPeaks(.,
                            minFrequency = proteinPercentPresence / 100) %>%
    MALDIquant::mergeMassPeaks(., 
                               method = "mean") %>%
    MALDIquant::trim(.,
                     c(lowerMassCutoff,
                       upperMassCutoff))
}


#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param db pool::() connection to the IDBac sqlite dataabse
#' @param fileshas the shas for the individual MALDIquant peak objects
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export


getProteinPeakData <-  function(db,dbConnection, fileshas){
  
  sqlQ <- glue::glue_sql("
                         SELECT `proteinPeaks`
                         FROM (SELECT *
                         FROM `IndividualSpectra`
                         WHERE (`spectrumSHA` IN ({shas*})))
                         WHERE (`proteinPeaks` IS NOT NULL)",
                         shas = fileshas,
                         .con = db
  )
  
  results <- DBI::dbSendQuery(dbConnection, sqlQ)
  results <- DBI::dbFetch(results)
  results <- unname(unlist(results, recursive = FALSE))
  unlist(lapply(results, function(x) unserialize(memDecompress(x, type= "gzip")))  )
  
}
