# Libraries that must be installed:
#library(dplyr)
#library(RSQLite)
#library(DBI)
#library(MALDIquant)

# inputs are:
# idbacPath    == idbacDirectory$filePath
# databasePath == input$selectedSearchLibrary        #.sqlite database path
# libResults  == librarySearchResults()


 graphingLibrarySearchResults<- function(idbacPath, databasePath, libResults){



# connect to user-specified database
dbcon <- DBI::dbConnect(RSQLite::SQLite(), databasePath)

# Connect dplyr to database
db <- dplyr::tbl(dbcon, "IDBacDatabase")




# Filter database by whatever metadata
# Return only rds column and strain ID
libSpec <- db %>%
  #filter(Strain_ID == "114A-2") %>%
  dplyr::select(c(Strain_ID,rds))




unknownProteinPeakFiles <- list.files(paste0(idbacPath, "\\Peak_Lists"),full.names = TRUE, pattern = "SummedProteinSpectra.rds")

 #Decompress blob
 libProteinPeaks <- memDecompress(libProteinPeaks[[1]][[1]], type="gzip")
 # Unserialize blob
 libProteinPeaks <- unserialize(libProteinPeaks, NULL)
 # Unlist rds file list
 libProteinPeaks <- unlist(libProteinPeaks, recursive = TRUE)
 # Return only protein peak MALDIquant objects
 libProteinPeaks <- libProteinPeaks[grep("ProteinPeaks", names(libProteinPeaks))]


