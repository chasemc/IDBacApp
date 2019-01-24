#' Create a pool connecton, given file names and a path
#' If there is no existing database by that name, create one.
#' 
#'   Note: Need both name and path due to how users select
#' @param fileName filename of sqlite database
#' @param filePath path of the location of sqlite database, sans filename
#'
#' @return a list of pool connections
#' @export
#' 
createPool <- function(fileName,
                       filePath){
  
  
  if(length(fileName) != length(filePath)){
    warning("createPool: Length of fileName and filePath are different")
    
  } else { 
    
    fileName <- tools::file_path_sans_ext(fileName)
    fileName <- paste0(fileName, ".sqlite")
    
    filePaths <- file.path(filePath, fileName)
    names(filePaths) <- tools::file_path_sans_ext(fileName)
    
    # If no current database by that name exists, create it, 
    # otherwise make a connection to the existing one.
    if(length(filePaths) == 0){
      con <- pool::dbPool(drv = RSQLite::SQLite(),
                          dbname = base::file.path(filePath, fileName))
    } else {
      con <- lapply(filePaths, function(x) pool::dbPool(drv = RSQLite::SQLite(),
                                                        dbname = x))
    }
    
    return(con)
  }
  
}