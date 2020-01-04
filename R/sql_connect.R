#' Create a pool connecton, given file names and a path
#'    If there is no existing database by that name, create one.
#'    createPool was the old name, kept to not break scripts
#' 
#'   Note: Need both name and path due to how users select
#' @param fileName filename of sqlite database
#' @param filePath path of the location of sqlite database, sans filename
#'
#' @return a list of pool connections
#' @export
#' 
createPool <- idbac_connect <- function(fileName,
                                        filePath){
  
  
  if (length(fileName) != length(filePath)) {
    warning("idbac_connect: Length of fileName and filePath are different")
    
  } else { 
    
    # Sanitize names TODO: add interactive y/n for change
    fileName <- IDBacApp::sanitize(fileName)
    fileName <- tools::file_path_sans_ext(fileName)
    fileName <- paste0(fileName, ".sqlite")
    filePaths <- file.path(filePath, fileName)
    names(filePaths) <- tools::file_path_sans_ext(fileName)
    
    
    if (length(filePaths) == 0) {
      
      con <- pool::dbPool(drv = RSQLite::SQLite(),
                          dbname = base::file.path(filePath, fileName))
      tried <- try(DBI::dbListTables(con),
                   silent = TRUE)
      req(class(tried) != "try-error")
      
    } else {
      
      con <- lapply(filePaths,
                    function(x){
                      pool::dbPool(drv = RSQLite::SQLite(),
                                   dbname = x)
                    })
      tried <- try(DBI::dbListTables(con[[1]]),
                   silent = TRUE)
      req(class(tried) != "try-error")
      
    }
    
    return(con)
  }
  
}






#----
#' Create New, Empty SQLite Database
#'
#' @param newExperimentName name for the sqlite DB
#' @param sqlDirectory directory to which the sqlite DB will be written into
#'
#' @return pool connection
#' @export
#'
createNewSQLITEdb <- function(newExperimentName,
                              sqlDirectory){
  # This pool is used when creating an entirely new "experiment" .sqlite db
  name <- IDBacApp::sanitize(newExperimentName)
  name <- gsub(" ", "", name)
  
  # max 50 character file length
  name <-  base::substr(name, 1, 50)
  
  pool <- IDBacApp::idbac_connect(name,
                                  sqlDirectory)
  return(pool)
}