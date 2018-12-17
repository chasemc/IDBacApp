#' Insert columns into IDBac SQLite metadata table
#' 
#' 
#' @param databaseName 
#' @param databasePath 
#' @param columnNames character vector of column names to be added 
#' @return a single trimmed and binned MALDIquant peak object




insertMetadataColumns <- function(pool,
                                  columnNames){
  
  conn <- pool::poolCheckout(pool)
  
  # Generate a string that is suitable for use in a query as a column or table name,
  # to generate valid SQL and protect against SQL injection attacks
  columnNames <- dbQuoteIdentifier(conn = conn,
                                   columnNames)
  
   samples <- glue::glue_sql("ALTER TABLE `metaData`
                              ADD {vars*}",
                            vars = columnNames, 
                           .con = conn)
 
 
  a <- DBI::dbSendStatement(conn, samples)
 message(a)
 
}

