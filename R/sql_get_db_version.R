#' Get the IDBac version that created the database
#'
#' @param pool pool object
#'
#' @return character version if present, or NULL if error
#' @export
#'
get_db_version <- function(pool){
  
  res <- pool::dbGetQuery(pool,
                          'Select idbac_version
                 FROM version')[[1]]
  
  tryCatch(pool::dbGetQuery(pool,
                            'Select idbac_version
                            FROM version')[[1]],
           error = function(x) NULL)
}