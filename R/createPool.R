#' Create a pool connecton, given file names and a path
#' If there is no existing database by that name, create one.
#' 
#' @param fileName filename of sqlite database
#' @param filePath path of the location of sqlite database, sans filename
#' @param userDBCon database connection (checked out pool)

#' @return a pool connection to the sqlite database
#' @export



createPool <- function(fileName,
                       filePath){
  # Filenames within directory
  names <- tools::file_path_sans_ext(base::list.files(filePath,
                                                pattern = ".sqlite",
                                                full.names = FALSE))
  # Filenames within directory including path
  namesNpaths <- base::list.files(filePath,
                                  pattern = ".sqlite",
                                  full.names = TRUE)
  # Return single file, including path
  filePaths <- namesNpaths[which(names == fileName)]
  
  # If no current database by that name exists, create it, 
  # otherwise make a connection to the existing one.
  if(length(filePaths) == 0){
    con <- pool::dbPool(drv = RSQLite::SQLite(),
                        dbname = base::file.path(filePath, fileName))
  } else {
    con <- pool::dbPool(drv = RSQLite::SQLite(),
                        dbname = filePaths)
  }
  
  return(con)
  
  
}