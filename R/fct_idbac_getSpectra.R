
#' Get spectra from database
#'
#' @param pool single pool
#' @param sampleID sample ID to retrieve
#' @param protein TRUE/FALSE to retrieve protein spectra
#' @param smallmol TRUE/FALSE to retrieve small molecule spectra
#'
#' @return single, averaged MALDIquant spectrum
#' @export
#'
idbac_get_spectra <- function(pool,
                              sampleID, 
                              protein = FALSE,
                              smallmol = FALSE){
  
  sampleID <- list(as.character(as.vector(sampleID)))
  
  if (!any(is.logical(protein), is.logical(smallmol))) {
    stop("idbac_get_spectra() protein and smallmol must be TRUE or FALSE")
  }
  if (length(unlist(list(protein, smallmol), recursive = TRUE)) != 2) {
    stop("idbac_get_spectra() protein and smallmol must be logical vectors of length 1")
  }
  proteinOrSmall <- NULL
  if (protein == TRUE) {
    proteinOrSmall <- ">"
  }
  if (smallmol == TRUE) {
    proteinOrSmall <- "<"
  }
  if(is.null(proteinOrSmall) || (sum(protein, smallmol) != 1)){
    stop("One of protein or smallmol must be TRUE and one must be FALSE")
  }
  
  
  result <-  pool::poolWithTransaction(pool, 
                                       function(conn){
                                         query <-  DBI::dbSendStatement(glue::glue("SELECT mass_index.mass_vector, spectra.spectrum_intensity
                                   FROM mass_index
                                   LEFT JOIN spectra
                                   ON mass_index.spectrum_mass_hash = spectra.spectrum_mass_hash
                                   WHERE strain_id == ?
                                   AND max_mass {proteinOrSmall} 6000"),
                                                                        con = conn)
                                         
                                         
                                         DBI::dbBind(query, sampleID)
                                         result <- DBI::dbFetch(query)
                                         DBI::dbClearResult(query)
                                         
                                         return(result)
                                       })
  if (nrow(result) > 0) {
    result <- lapply(1:nrow(result),
                     function(x){
                       cbind(IDBacApp::deserial(rawToChar(fst::decompress_fst(result[x, 1][[1]]))),
                             IDBacApp::deserial(rawToChar(fst::decompress_fst(result[x, 2][[1]]))))
                     })
    
    
    result <- lapply(result,
                     function(x){
                       MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                      intensity = x[ , 2])
                     })
  } else {
    result <- list()
  }
  
  return(result)
  
}