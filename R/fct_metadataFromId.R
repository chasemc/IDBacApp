#' Extract a metadata column
#'
#' @param strainID strain_id
#' @param metadataColumn metadata column name ('species')
#' @param pool pool connection
#'
#' @return data.frame with two columns 'strain_id' and  whatever metadataColumn is requested; rows filtered by ID
#' @export
#'
idbac_get_metadata <- function(strainID = NULL,
                               metadataColumn = NULL,
                               pool) {
  .checkPool(pool = pool)
  if (is.null(strainID) && is.null(metadataColumn)) {
    pool::poolWithTransaction(
      pool,
      function(conn) {
        statement <- glue::glue(
          "SELECT *
                                FROM metadata"
        )
        query <- DBI::dbSendStatement(
          statement = statement,
          con = conn
        )
        selectedMeta <- DBI::dbFetch(query)
        DBI::dbClearResult(query)
        return(selectedMeta)
      }
    )
  } else if (is.null(strainID)) {
    pool::poolWithTransaction(
      pool,
      function(conn) {
        glued <- c("strain_id", metadataColumn)
        query <- glue::glue_sql(
          "SELECT {`glued`*}
                                FROM metadata
                                WHERE `strain_id` = $ids",
          .con = conn
        )
        DBI::dbBind(query, list(ids = strainID))
        selectedMeta <- DBI::dbFetch(query)
        DBI::dbClearResult(query)
        return(selectedMeta)
      }
    )
  } else if (is.null(metadataColumn)) {
    pool::poolWithTransaction(
      pool,
      function(conn) {
        statement <- glue::glue(
          "SELECT *
                                FROM metadata
                                WHERE `strain_id` = $ids"
        )
        query <- DBI::dbSendStatement(
          statement = statement,
          con = conn
        )
        DBI::dbBind(query, list(ids = strainID))
        selectedMeta <- DBI::dbFetch(query)
        DBI::dbClearResult(query)
        return(selectedMeta)
      }
    )
  } else {
    pool::poolWithTransaction(
      pool,
      function(conn) {
        glued <- c("strain_id", metadataColumn)
        statement <- glue::glue_sql(
          "SELECT {`glued`*}
                                FROM metadata
                                WHERE `strain_id` = $ids",
          .con = conn
        )
        query <- DBI::dbSendStatement(
          statement = statement,
          con = conn
        )
        DBI::dbBind(query, list(ids = strainID))
        selectedMeta <- DBI::dbFetch(query)
        DBI::dbClearResult(query)
        return(selectedMeta)
      }
    )
  }
}
