






#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param pool 
#' @param whetherProtein
#'
#' @return
#' @export
#'
#' @examples
availableSampleNames <- function(pool, whetherProtein, allSamples){
  
  
  conn <<- pool::poolCheckout(pool)
  
  if (allSamples == TRUE) {
  
    query <- glue::glue_sql("
                          SELECT DISTINCT `Strain_ID`
                            FROM `IndividualSpectra`",
                            .con = conn)
  } else {
  if (whetherProtein == TRUE) {
    query <- glue::glue_sql("
                          SELECT DISTINCT `Strain_ID`
                          FROM `IndividualSpectra`
                          WHERE (`proteinPeaks` IS NOT NULL)",
                            .con = conn)
    
  } else {
    
    query <- glue::glue_sql("
                         SELECT DISTINCT `Strain_ID`
                         FROM `IndividualSpectra`
                         WHERE (`smallMoleculePeaks` IS NOT NULL)",
                            .con = conn)
    
  }
    }
    
  query <- DBI::dbGetQuery(conn, query)
  return(query[ , 1])
  
  
}
