#' Retrieve MALDIquant peak objects from an IDBac sqlite database
#'
#' @param pool sqlite pool
#' @param ids filter retrieved peaks by sample ID (character vector, or NULL to return all)
#' @param type "protein",  "small", or "all"
#' @param minSNR minimum SNR a a peak must have to be retained
#' @return unlisted MALDIquant peak objects correspoding to the provided fileshas
#' @export
#'
.retrieve_peaks_from_pool <- function(pool,
                                      ids,
                                      type,
                                      minSNR) {
  if (!inherits(type, "character")) stop("Provided value for 'type' wasn't character.")
  if (!inherits(ids, c("character", "NULL"))) stop("Provided value for 'sampleIDs' wasn't character or NULL.")
  .checkPool(pool)

  conn <- pool::poolCheckout(pool = pool)
  if (inherits(ids, "character")) {
    sym <- "" # Just to be safe with the SQL
    switch(type,
      "protein" = assign("sym", "AND max_mass > 6000"),
      "small" = assign("sym", "AND max_mass < 6000"),
      "all" = assign("sym", "")
    )
    query <- DBI::dbSendStatement(glue::glue("SELECT strain_id, peak_matrix
                                           FROM spectra
                                           WHERE (strain_id = ?)
                                           {sym}"),
      con = conn
    )
    DBI::dbBind(query, list(as.character(as.vector(ids))))
    results <- DBI::dbFetch(query)
    DBI::dbClearResult(query)
  } else if (is.null(ids)) {
    sym <- "" # Just to be safe with the SQL
    switch(type,
      "protein" = assign("sym", "WHERE max_mass > 6000"),
      "small" = assign("sym", "WHERE max_mass < 6000"),
      "all" = assign("sym", "")
    )
    results <- DBI::dbGetQuery(glue::glue("SELECT strain_id, peak_matrix
                                            FROM spectra
                                            {sym}"))
  }

  pool::poolReturn(conn)
  .parse_json_peaks(
    results,
    minSNR
  )
}



#' Create maldiquant peaks from IDBac SQL peak JSON
#'
#' @param input IDBac SQL strain_id ancd peak json
#' @param minSNR minimum SNR a a peak must have to be retained
#'
#' @return list of maldiquant peak objects
.parse_json_peaks <- function(input,
                              minSNR) {
  len <- nrow(input)
  collapsed <- paste0(input[, 2], collapse = " ")
  subby <- strsplit(
    substring(
      collapsed,
      as.integer(gregexpr(
        "\\[",
        collapsed
      )[[1]]) + 1,
      as.integer(gregexpr(
        "\\]",
        collapsed
      )[[1]]) - 1
    ),
    ",",
    fixed = T
  )

  z <- lapply(
    seq(1L, len, 1L),
    function(x) {
      suppressWarnings( # will warn when converting to NA
        # subby is 3*number of spectra: mass, int, snr, mass, int, snr
        x <- lapply(subby[c(c(1, 2, 3) + (3 * (x - 1)))], as.numeric)
      )
      if (any(sapply(x[2:3], function(x) is.na(x[[1]])))) {
        # Account for weird exception of NA peaks
        for (i in 2:3) {
          x[[i]][is.na(x[[i]])] <- 0
        }
      }
      ind <- x[[3]] >= minSNR
      MALDIquant::createMassPeaks(
        mass = x[[1]][ind],
        intensity = x[[2]][ind],
        snr = x[[3]][ind]
      )
    }
  )
  names(z) <- input[, 1]
  split(z, names(z))
}
