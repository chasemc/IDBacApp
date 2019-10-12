


#' Extract a metadata column 
#'
#' @param id Strain_ID
#' @param metadataColumn metadata column name ('Species')
#' @param pool pool connection
#'
#' @return data.frame with two columns 'Strain_ID' and  whatever metadataColumn is
#' @export
#'
metadata_from_id <- function(id,
                             metadataColumn,
                             pool) {
  
  
  conn <- pool::poolCheckout(pool)

  
  a <- glue::glue("SELECT '{metadataColumn}'
                                  FROM metaData
                                  WHERE `Strain_ID` = $ids")
  
  
  query <- DBI::dbSendStatement(b,
                                con=conn)
  
  DBI::dbBind(query, list(ids = id))
  selectedMeta <- DBI::dbFetch(query)
  
  DBI::dbClearResult(query)
  
  conn <- pool::poolReturn(pool)
  
}