#' Search an IDBac database to see which sample IDs have protein or small molecule data
#'
#' @param type "protein", "small",
#' @param pool single IDBac pool connection
#'
#' @return vector of sample names in database with protein or small mol spectra
#' @export
#'
idbac_available_samples <- function(pool,
                                    type) {
  if (!inherits(type, "character")) stop("Provided value for 'type' wasn't character.")
  switch(type,
    "protein" = assign("sym", "WHERE max_mass > 6000"),
    "small" = assign("sym", "WHERE max_mass < 6000"),
    "all" = assign("sym", "")
  )

  query <- glue::glue("SELECT DISTINCT `strain_id`
                         FROM `spectra`
                         {sym}")
  query <- DBI::dbGetQuery(pool, query)
  return(query[, 1])
}
