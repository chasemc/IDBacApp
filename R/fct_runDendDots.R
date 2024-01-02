#' Run Dend and dots
#'
#' @param rawDendrogram NA
#' @param trimdLabsDend NA
#' @param pool  NA
#' @param columnID NA
#' @param colors  NA
#' @param text_shift NA
#'
#' @return NA
#'
#'
runDendDots <- function(rawDendrogram, trimdLabsDend, pool, columnID, colors, text_shift) {
  conn <- pool::poolCheckout(pool)
  dendLabs <- labels(rawDendrogram)

  query <- DBI::dbSendStatement("SELECT *
                                  FROM metadata
                                  WHERE `strain_id` = ?",
    con = conn
  )
  DBI::dbBind(query, list(dendLabs))
  selectedMeta <- DBI::dbFetch(query)
  DBI::dbClearResult(query)

  selectedMeta <- selectedMeta[, colnames(selectedMeta) %in% columnID]
  uniq <- unique(selectedMeta)
  selectedMeta <- sapply(uniq, function(x) selectedMeta %in% x)

  for (i in seq_along(colors)) {
    selectedMeta[, i][which(selectedMeta[, i] == TRUE)] <- colors[[i]]
  }
  selectedMeta[selectedMeta == FALSE] <- "#00000000"

  colnames(selectedMeta) <- uniq
  colored_dots(selectedMeta,
    trimdLabsDend,
    horiz = T,
    # rowLabels = uniq,
    sort_by_labels_order = FALSE,
    text_shift = text_shift
  )
}
