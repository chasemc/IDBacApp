
#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param pool sqlite pool
#' @param sampleIDs sample IDs of samples to process
#' @param protein whether to search SQL for protein or small mol spectra
#'
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export


getPeakData <-  function(pool, sampleIDs, protein){
  
  if (!is.logical(protein)) {stop("In getPeakData, provided value for 'protein' wasn't logical-type.")}
  
  if (protein == TRUE) {
    sym <- '>'
  } else {
    sym <- '<'
  }  
  
  
  checkSinglePool(pool)
  
  shiny::validate({
    need(is.character(sampleIDs), 
         glue::glue("Expected sampleIDs to be a character object, received: {class(sampleIDs)}"))
  })
  
  conn <- pool::poolCheckout(pool = pool)
  
  query <- DBI::dbSendStatement(glue::glue("SELECT Strain_ID, peakMatrix
                              FROM IndividualSpectra
                                  WHERE maxMass {sym} 6000
                                  AND (Strain_ID = ?)"),
                                con = conn)
  
  DBI::dbBind(query, list(as.character(as.vector(sampleIDs))))
  
  
  results <- DBI::dbFetch(query)
  DBI::dbClearResult(query)
  pool::poolReturn(conn)
  
  samps <- unique(results$Strain_ID)
  results <- lapply(samps,
                    function(x){
                      unname(lapply(which(results$Strain_ID == x), function(y){
                        
                        z <- jsonlite::fromJSON(results$peakMatrix[[y]])
                        
                        MALDIquant::createMassPeaks(mass = z$mass,
                                                    intensity = z$intensity ,
                                                    snr = as.numeric(z$snr))
                      }))
                      
                    }
  )
  
  names(results) <- samps
  
  
  
  return(results)
  
  
}