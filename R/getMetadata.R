


#' Extract a metadata column 
#'
#' @param strainID strain_id
#' @param metadataColumn metadata column name ('Species')
#' @param pool pool connection
#'
#' @return data.frame with two columns 'strain_id' and  whatever metadataColumn is requested; rows filtered by ID
#' @export
#'
metadata_from_id <- function(strainID,
                             metadataColumn,
                             pool) {
  
  
  conn <- pool::poolCheckout(pool)
  
  metadataColumn <- paste("strain_id", metadataColumn, sep = ", ")
  
  a <- glue::glue("SELECT {metadataColumn}
                   FROM metaData
                   WHERE `strain_id` = $ids")
  
  
  query <- DBI::dbSendStatement(a,
                                con = conn)
  
  DBI::dbBind(query, list(ids = strainID))
  selectedMeta <- DBI::dbFetch(query)
  
  DBI::dbClearResult(query)
  
  pool::poolReturn(conn)
  return(selectedMeta)
  
}