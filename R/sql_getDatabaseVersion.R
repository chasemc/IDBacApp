#' Get the IDBac version that created the database
#'
#' @param pool pool object
#'
#' @return numeric_version if present, or NULL if error
#' @export
#'
idbac_db_version <- function(pool) {
  res <- pool::dbGetQuery(
    pool,
    "Select idbac_version
                 FROM version"
  )[[1]]
  return(numeric_version(res))
}
