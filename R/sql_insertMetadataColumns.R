#' Insert columns into IDBac SQLite metadata table
#'
#' @param pool pool
#' @param columnNames character vector of column names to be added
#'
#' @return a single trimmed and binned MALDIquant peak object
#'
insertMetadataColumns <- function(pool,
                                  columnNames) {
  conn <- pool::poolCheckout(pool)
  warning("Trying to insert column: ", columnNames, " into: ", conn@dbname)
  # Generate a string that is suitable for use in a query as a column or table name,
  # to generate valid SQL and protect against SQL injection attacks
  quotedColumnNames <- DBI::dbQuoteIdentifier(
    conn = conn,
    columnNames
  )
  samples <- glue::glue_sql("ALTER TABLE `metadata`
                              ADD {vars*} TEXT",
    vars = quotedColumnNames,
    .con = conn
  )
  tryCatch(DBI::dbSendStatement(conn, samples),
    error = function(x) {
      warning(paste(
        "Tried, but didn't add ",
        quotedColumnNames,
        " column to ",
        strsplit(
          basename(conn@dbname),
          ".sqlite"
        )[[1]],
        " metadata, it might already be present."
      ))
    },
    finally = function(x) warning(as.character(x@sql))
  )
  checkMetaColumns <- base::colnames(DBI::dbGetQuery(
    conn = conn,
    "SELECT * FROM metadata",
    n = 1
  ))
  checker <- columnNames %in% checkMetaColumns
  pool::poolReturn(conn)
  if (checker) {
    warning(columnNames, " was inserted or was already present in ", conn@dbname)
  } else {
    warning(columnNames, " was unable to be inserted in", conn@dbname)
  }
}
