
#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param pool sqlite pool
#' @param sampleIDs sample IDs of samples to process
#' @param protein whether to search SQL for protein or small mol spectra
#'
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export
#' 
.retrieve_peaks_from_pool <-  function(pool, sampleIDs, protein){
  
  if (!is.logical(protein)) {stop("Provided value for 'protein' wasn't logical-type.")}
  
  if (protein == TRUE) {
    sym <- '>'
  } else {
    sym <- '<'
  }  
  
  .checkPool(pool)
  
  shiny::validate({
    need(is.character(sampleIDs), 
         glue::glue("Expected sampleIDs to be a character object, received: {class(sampleIDs)}"))
  })
  
  conn <- pool::poolCheckout(pool = pool)
  
  query <- DBI::dbSendStatement(glue::glue("SELECT strain_id, peak_matrix
                              FROM spectra
                                  WHERE max_mass {sym} 6000
                                  AND (strain_id = ?)"),
                                con = conn)
  
  DBI::dbBind(query, list(as.character(as.vector(sampleIDs))))
  
  
  results <- DBI::dbFetch(query)
  DBI::dbClearResult(query)
  pool::poolReturn(conn)
  
  samps <- unique(results$strain_id)
  results <- lapply(samps,
                    function(x){
                      unname(lapply(which(results$strain_id == x), function(y){
                        z <- jsonlite::fromJSON(results$peak_matrix[[y]])
                        if (any(lengths(z) == 0)) {
                          MALDIquant::createMassPeaks(mass = numeric(0),
                                                      intensity = numeric(0) ,
                                                      snr = numeric(0))
                        } else {
                          MALDIquant::createMassPeaks(mass = z$mass,
                                                      intensity = z$intensity ,
                                                      snr = as.numeric(z$snr))
                        }
                      }))
                      
                    }
  )
  
  names(results) <- samps
  
  
  
  return(results)
  
  
}