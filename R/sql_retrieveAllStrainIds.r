#' retrieveAllStrainIds
#'
#' @param databaseConnection NA
#' @param table NA
#'
#' @return NA
#'
#'

retrieveAllStrainIds <- function(databaseConnection, table) {
  # Gets unique strain_ids given a RSQLite database connecction and table name
  conn <- pool::poolCheckout(databaseConnection)


  dbQuery <- glue::glue_sql("SELECT DISTINCT `strain_id`
                            FROM ({tab*})",
    tab = table,
    .con = conn
  )

  dbQuery <- DBI::dbSendQuery(conn, dbQuery)
  pool::poolReturn(conn)
  DBI::dbFetch(dbQuery)[, 1]
}
