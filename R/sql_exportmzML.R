#' Export mzML/mzXML from SQLite DB
#'
#' @param pool pool connection, not-checked-out
#' @param sampleIDs sample IDs
#' @param saveToDir directory files should be saved to
#'
#' @return NA, files written
#'
#' @importFrom DBI dbSendStatement dbBind dbHasCompleted dbFetch dbClearResult
#' @importFrom pool poolReturn
#' @importFrom glue glue
exportmzML <- function(pool, sampleIDs, saveToDir) {
  conn <- pool::poolCheckout(pool)
  query <- dbSendStatement("SELECT xml.xml,spectra.strain_id
FROM `xml`
LEFT JOIN `spectra`
ON xml.xml_hash = spectra.xml_hash
WHERE strain_id == ?
GROUP BY strain_id;
", con = conn)
  dbBind(query, list(as.character(as.vector(sampleIDs))))
  inc <- 100 / length(sampleIDs) / 5
  counter <- 0
  while (!dbHasCompleted(query)) {
    chunk <- dbFetch(query, 5)
    counter <- counter - inc
    ids <- chunk$strain_id
    ids <- base::make.unique(IDBacApp:::sanitize(ids))
    ids <- gsub("\\.", "_", ids)
    fileLoc <- base::file.path(saveToDir, ids)
    chunk <- lapply(chunk$xml, function(x) {
      x <- decompress(unlist(x))
      rawToChar(x)
    })
    for (i in seq_along(ids)) {
      writeLines(unlist(chunk[[i]]), fileLoc[[i]])
      fl <- readLines(fileLoc[[i]], n = 20)
      if (any(grepl("<mzML", fl, ignore.case = TRUE))) {
        whichMZ <- "mzML"
      } else if (any(grepl("<mzXML", fl, ignore.case = TRUE))) {
        whichMZ <- "mzXML"
      } else {
        stop("exportmzML() wasn't mzXML or mzML")
      }
      file.rename(fileLoc, paste0(
        fileLoc, ".",
        whichMZ
      ))
      warning(glue("Exported: {ids[[i]]}"))
    }
  }
  dbClearResult(query)
  poolReturn(conn)
}
