




#' getAllStrain_IDsfromSQL
#'
#' @param databaseConnection NA
#' @param table NA
#'
#' @return NA
#' @export
#'
#' @examples NA
getAllStrain_IDsfromSQL <- function(databaseConnection, table){
  # Gets unique Strain_IDs given a RSQLite database connecction and table name
  
  dbQuery <- glue::glue_sql("SELECT DISTINCT `Strain_ID`
                            FROM ({tab*})",
                            tab = table,
                            .con = databaseConnection)
  
  conn <- pool::poolCheckout(databaseConnection)
  dbQuery <- DBI::dbSendQuery(conn, dbQuery)
  dbQuery <- DBI::dbFetch(dbQuery)[ , 1]
  
  
}





