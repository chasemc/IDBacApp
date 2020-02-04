


#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param whetherProtein T/F protein spectra (T), small mol (F)
#' @param checkedPool single IDBac pool connection 
#' @param allSamples both protein and small mol? Takes precedence over whetherProtein
#'
#' @return vector of sample names in database with protein or small mol spectra
#' @export
#'
idbac_available_samples <- function(pool, 
                                    whetherProtein, 
                                    allSamples){
  
  
  if (allSamples == TRUE) {
  
    query <- glue::glue_sql("
                          SELECT DISTINCT `strain_id`
                            FROM `spectra`",
                            .con = pool)
  } else {
  if (whetherProtein == TRUE) {
    query <- glue::glue_sql("
                          SELECT DISTINCT `strain_id`
                          FROM `spectra`
                          WHERE max_mass > 6000",
                            .con = pool)
    
  } else {
    
    query <- glue::glue_sql("
                         SELECT DISTINCT `strain_id`
                         FROM `spectra`
                         WHERE max_mass < 6000",
                            .con = pool)
    
  }
    }
    
  query <- DBI::dbGetQuery(pool, query)
  return(query[ , 1])
  
  
}
