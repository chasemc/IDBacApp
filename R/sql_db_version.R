# version table -----------------------------------------------------------

#' Create version table
#'
#' @param pool sqlite pool
#'
#' @return writes to sqlite database
#'
#'
sql_fill_version_table <- function(pool) {
  pool::poolWithTransaction(
    pool = pool,
    func = function(conn) {
      ver <- cbind.data.frame(
        idbac_version = as.character(utils::packageVersion("IDBacApp")),
        r_version = as.character(serial(utils::sessionInfo()$R.version)),
        db_version = current_db_version()
      )
      # Add version table
      DBI::dbWriteTable(
        conn = conn,
        name = "version", # SQLite table to insert into
        value = ver, # Insert single row into DB
        append = TRUE, # Append to existing table
        overwrite = FALSE
      ) # Do not overwrite
    }
  )
}
