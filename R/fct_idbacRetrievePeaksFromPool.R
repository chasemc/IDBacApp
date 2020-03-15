
#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param pool sqlite pool
#' @param sampleIDs sample IDs of samples to process
#' @param protein whether to search SQL for protein or small mol spectra
#' @param minSNR minimum SNR a a peak must have to be retained
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export
#' 
.retrieve_peaks_from_pool <-  function(pool, sampleIDs, protein, minSNR){
  if (!is.logical(protein)) {stop("Provided value for 'protein' wasn't logical-type.")}
  
  if (protein == TRUE) {
    sym <- '>'
  } else {
    sym <- '<'
  }  
  
  IDBacApp:::.checkPool(pool)
  
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
  
  .parse_json_peaks(results,
                    minSNR)
  
  
}







#' Create maldiquant peaks from IDBac SQL peak JSON 
#'
#' @param input IDBac SQL strain_id ancd peak json
#' @param minSNR minimum SNR a a peak must have to be retained
#' 
#' @return list of maldiquant peak objects
.parse_json_peaks <- function(input,
                              minSNR){
  
  len <- nrow(input)
  collapsed <- paste0(input[,2], collapse = " ")
  
  subby <- strsplit(substring(collapsed,
                              as.integer(gregexpr("\\[",
                                                  collapsed)[[1]]) + 1,
                              as.integer(gregexpr("\\]",
                                                  collapsed)[[1]]) - 1),
                    ",",
                    fixed = T)
  
  
  z <- lapply(seq(1L, len, 1L),
              function(x){
                
                x <- lapply(subby[c(c(1, 2, 3) + (3 * (x - 1)))], as.numeric)
                
                ind <- x[[3]] >= minSNR
                
                MALDIquant::createMassPeaks(mass = x[[1]][ind],
                                            intensity = x[[2]][ind],
                                            snr = x[[3]][ind])
                
              })
  names(z) <- input[ , 1]
  split(z, names(z))
  
}


