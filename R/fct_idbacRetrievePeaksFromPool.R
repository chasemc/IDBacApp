
#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#' 
#' @param pool sqlite pool
#' @param ids filter retrieved peaks by sample ID (character vector, or NULL to return all)
#' @param type "protein",  "small", or "all"
#' @param minSNR minimum SNR a a peak must have to be retained
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export
#' 
.retrieve_peaks_from_pool <-  function(pool, 
                                       ids,
                                       type,
                                       minSNR){

   if (!inherits(type, "character")) stop("Provided value for 'type' wasn't character.")
  if (!inherits(ids, c("character", "NULL"))) stop("Provided value for 'sampleIDs' wasn't character or NULL.")
  .checkPool(pool)
  
  switch(type,
         "protein" = assign("sym", "WHERE max_mass > 6000"),
         "small" = assign("sym", "WHERE max_mass < 6000"),
         "all" = assign("sym", ""))
  
  conn <- pool::poolCheckout(pool = pool)
  
  if (inherits(ids, "character")) {
    
    query <- DBI::dbSendStatement(glue::glue("SELECT strain_id, peak_matrix
                                           FROM spectra
                                           {sym}
                                           AND (strain_id = ?)"),
                                  con = conn)
    DBI::dbBind(query, list(as.character(as.vector(ids))))
    results <- DBI::dbFetch(query)
    DBI::dbClearResult(query)
    
  } else if (is.null(ids)) {
    
    results <- DBI::dbGetQuery(glue::glue("SELECT strain_id, peak_matrix
                                            FROM spectra
                                            {sym}"))
  }
  
  
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


