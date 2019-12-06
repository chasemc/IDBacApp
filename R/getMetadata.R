


#' Extract a metadata column 
#'
#' @param strainID Strain_ID
#' @param metadataColumn metadata column name ('Species')
#' @param pool pool connection
#'
#' @return data.frame with two columns 'Strain_ID' and  whatever metadataColumn is requested; rows filtered by ID
#' @export
#'
metadata_from_id <- function(strainID,
                             metadataColumn,
                             pool) {
  
  
  conn <- pool::poolCheckout(pool)
  
  metadataColumn <- paste("Strain_ID", metadataColumn, sep = ", ")
  
  a <- glue::glue("SELECT {metadataColumn}
                   FROM metaData
                   WHERE `Strain_ID` = $ids")
  
  
  query <- DBI::dbSendStatement(a,
                                con = conn)
  
  DBI::dbBind(query, list(ids = strainID))
  selectedMeta <- DBI::dbFetch(query)
  
  DBI::dbClearResult(query)
  
  pool::poolReturn(conn)
  return(selectedMeta)
  
}