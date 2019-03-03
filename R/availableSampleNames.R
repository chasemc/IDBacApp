


#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param whetherProtein T/F protein spectra (T), small mol (F)
#' @param checkedPool checkedPool 
#' @param allSamples both protein and small mol? Takes precedence over whetherProtein
#'
#' @return vector of sample names in database with protein or small mol spectra
#' @export
#'
availableSampleNames <- function(checkedPool, 
                                 whetherProtein, 
                                 allSamples){
  
  
  if (allSamples == TRUE) {
  
    query <- glue::glue_sql("
                          SELECT DISTINCT `Strain_ID`
                            FROM `IndividualSpectra`",
                            .con = checkedPool)
  } else {
  if (whetherProtein == TRUE) {
    query <- glue::glue_sql("
                          SELECT DISTINCT `Strain_ID`
                          FROM `IndividualSpectra`
                          WHERE maxMass > 6000",
                            .con = checkedPool)
    
  } else {
    
    query <- glue::glue_sql("
                         SELECT DISTINCT `Strain_ID`
                         FROM `IndividualSpectra`
                         WHERE maxMass < 6000",
                            .con = checkedPool)
    
  }
    }
    
  query <- DBI::dbGetQuery(checkedPool, query)
  return(query[ , 1])
  
  
}
