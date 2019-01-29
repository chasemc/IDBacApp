


#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param pool 
#' @param whetherProtein
#'
#' @return
#' @export
#'
#' @examples
availableSampleNames <- function(checkedPool, whetherProtein, allSamples){
  
  
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
                          WHERE (`proteinPeaks` IS NOT NULL)",
                            .con = checkedPool)
    
  } else {
    
    query <- glue::glue_sql("
                         SELECT DISTINCT `Strain_ID`
                         FROM `IndividualSpectra`
                         WHERE (`smallMoleculePeaks` IS NOT NULL)",
                            .con = checkedPool)
    
  }
    }
    
  query <- DBI::dbGetQuery(checkedPool, query)
  return(query[ , 1])
  
  
}
