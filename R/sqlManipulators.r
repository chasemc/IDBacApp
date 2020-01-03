




#' getAllstrain_idsfromSQL
#'
#' @param databaseConnection NA
#' @param table NA
#'
#' @return NA
#' @export
#'

getAllstrain_idsfromSQL <- function(databaseConnection, table){
  # Gets unique strain_ids given a RSQLite database connecction and table name
  
  dbQuery <- glue::glue_sql("SELECT DISTINCT `strain_id`
                            FROM ({tab*})",
                            tab = table,
                            .con = databaseConnection)
  
  conn <- pool::poolCheckout(databaseConnection)
  dbQuery <- DBI::dbSendQuery(conn, dbQuery)
  dbQuery <- DBI::dbFetch(dbQuery)[ , 1]
  
  
}





