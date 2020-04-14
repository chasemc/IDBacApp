
#' Get spectra from database
#'
#' @param pool single pool
#' @param sample_ids sample ID to retrieve
#' @param type "protein",  "small", or "all"
#' @param MALDIquant TRUE/FALSE data.table or MALDIquant spectrum
#'
#' @return single, averaged MALDIquant spectrum
#' @export
#'
idbac_get_spectra <- function(pool,
                              sample_ids, 
                              type,
                              MALDIquant = TRUE){
  
  sample_ids <- list(as.character(as.vector(sample_ids)))
  

  if (!is.logical(MALDIquant)) {
    stop("idbac_get_spectra() MALDIquant must be logical vectors of length 1")
  }
  if (length(MALDIquant) != 1L) {
    stop("idbac_get_spectra() MALDIquant must be logical vectors of length 1")
    
  }
  .checkPool(pool)
  if (!inherits(type, "character")) stop("Provided value for 'type' wasn't character.")
  
  sym <- "" # Just to be safe with the SQL
  switch(type,
         "protein" = assign("sym", "AND max_mass > 6000"),
         "small" = assign("sym", "AND max_mass < 6000"),
         "all" = assign("sym", ""))
  
  
  result <-  pool::poolWithTransaction(pool, 
                                       function(conn){
                                         query <-  DBI::dbSendStatement(glue::glue("SELECT spectra.strain_id, mass_index.mass_vector, spectra.spectrum_intensity
                                   FROM mass_index
                                   LEFT JOIN spectra
                                   ON mass_index.spectrum_mass_hash = spectra.spectrum_mass_hash
                                   WHERE strain_id == ?
                                   {sym}"),
                                                                        con = conn)
                                         
                                         DBI::dbBind(query, sample_ids)
                                         result <- DBI::dbFetch(query)
                                         DBI::dbClearResult(query)
                                         
                                         return(result)
                                       })
  
  if (nrow(result) > 0) {
    result <- data.table::as.data.table(result)
    ids <- result$strain_id
    
    result <- lapply(seq_along(ids),
                     function(i){
                       data.table::data.table(mass = double_from_raw_compressed(result[i,][[2]][[1]]),
                                              intensity = double_from_raw_compressed(result[i,][[3]][[1]]))
                     })
    if (MALDIquant) {
      
      result <- lapply(result,
                       function(x){
                         MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                        intensity = x[ , 2])
                       })
    }
    
    names(result) <- ids
    
  } else {
    result <- list()
  }
  
  return(result)
  
}