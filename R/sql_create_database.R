

#' Title
#'
#' @param fileName file name for new IDBac database
#' @param filePath path where database will be created
#'
#' @return path to new database
#' @export
#'
idbac_create <- function(fileName,
                         filePath){
  
  if (!is.character(fileName)) {
    stop("db_create(fileName) expected to be character")
  }
  
  if (!is.character(filePath)) {
    stop("db_create(filePath) expected to be character")
  }
  
  if (sum(length(fileName), length(filePath)) < 2L) {
    stop("db_create(fileName) and db_create(filePath) must be same length") 
  }
  
  # TODO: remove stop and make interactive
  
  if (!identical(sanitize(fileName), fileName)) {
    stop("db_create(fileName) must be a valid file name, run sanitize(fileName) first") 
  }
  
  fileName <- paste0(fileName, 
                     ".sqlite")
  
  filePaths <- file.path(filePath,
                         fileName)
  
  names(filePaths) <- tools::file_path_sans_ext(fileName)
  
  
  pool <- pool::dbPool(drv = RSQLite::SQLite(),
                       dbname = base::file.path(filePath, fileName))
  
  conn <- pool::poolCheckout(pool)
  
  # Add tables
  sql_create_spectra_table(conn)
  sql_create_massindex_table(conn)
  sql_create_metadata_table(conn)
  sql_create_xml_table(conn)
  sql_create_version_table(conn)
  sql_create_locale_table(conn)
  
  pool::poolReturn(conn)
  
  # Fill IDBac version 
  sql_fill_version_table(pool)
  # Fill locale 
  sql_fill_locale_table(pool)
  
  pool::poolClose(pool)
  
  #TODO: add check
  
  return(normalizePath(base::file.path(filePath, fileName), 
                       winslash = "/",
                       mustWork = F))
}

