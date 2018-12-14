

# Create a pool, given file names and a path
# Return single Pool
# Requires pool package


createPool <- function(fileName,
                       filePath){
# Filenames within directory
names <- tools::file_path_sans_ext(list.files(filePath,
                                                  pattern = ".sqlite",
                                                  full.names = FALSE))
# Filenames within directory including path
namesNpaths <- base::list.files(filePath,
                        pattern = ".sqlite",
                        full.names = TRUE)
# Return single file, including path
filePaths <- filePaths[which(names == fileName)]

con <- pool::dbPool(drv = RSQLite::SQLite(),
             dbname = filePaths)

return(con)


}