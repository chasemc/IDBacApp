

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
  
  if (!identical(IDBacApp::sanitize(fileName), fileName)) {
    stop("db_create(fileName) must be a valid file name, run IDBacApp::sanitize(fileName) first") 
  }
  
  fileName <- paste0(fileName, 
                     ".sqlite")
  
  filePaths <- file.path(filePath,
                         fileName)
  
  names(filePaths) <- tools::file_path_sans_ext(fileName)
  
  
  con <- pool::dbPool(drv = RSQLite::SQLite(),
                      dbname = base::file.path(filePath, fileName))
  
  
  
  
  
  
  
  
  
  
  
  tried <- try(DBI::dbListTables(con),
               silent = TRUE)
  
  
  
  
  
  
  
  req(class(tried) != "try-error")
  
  
}

