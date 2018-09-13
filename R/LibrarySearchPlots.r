

# Packages that must be installed:
#library(dplyr)
#library(RSQLite)
#library(DBI)
#library(MALDIquant)

# output:

# A three-column tibble with columns:

#"unknownStrainIDs" "librarySpectrum" "score"


# unknown librarySpectrum        score
# 1  114A-1          114C-6 1.560207e-01
# 2  114A-2          114A-2 2.220446e-16



databaseSearch <- function(idbacPath, databasePath, wantReport){


  # Inputs:
  # idbacPath     == idbacDirectory$filePath
  # databasePath  == input$selectedSearchLibrary        #.sqlite database path
  # wantReport == Logical     if TRUE == make ggplots here... will be slower  if == FALSE don't make ggplots... faster
  # Returns:
  # Returns list of top hit matches. Each list element is one unknown sample, with one best cosine similarity score + lib sample ID



  # This is the workhorse of the searching.
  # Takes a list of unknown *Sample IDs* as input

  searchLibrary <- function(unk){

    # Process single unknown sample spectrum
    unk <- unknownProcessing(unk)
    unkl <- unk@metaData$Strain

    # Perform cosine similarity search across all library spectra
    lapply(libStrainIDs, function(lib){

      # Process single library strain
      qq <- getLibraryProteinSpectrum(singleLibSpec = lib,
                                      dbConnection = dbcon)

      lab <- list(unknown = unkl, library = qq@metaData$Strain)
      # Bin one unknown and one library spectra (strict = one peak per bin)
      a <- MALDIquant::binPeaks(c(qq, unk), tolerance = .02, method = "relaxed")
      remove(qq, unk)
      # Turn into a matrix, rows = samples, columns = binned peaks, cells = peak intensity
      a <- MALDIquant::intensityMatrix(a)

      # Get sample names of the two spectra

      # Replace NA values with 0
      a[is.na(a)] <- 0
      d <- coop::tcosine(a)[[2]]
      cbind.data.frame(Unknown = unkl , Lib_ID = lab$library, Score = as.numeric(d))
    })

  }

  # Get "Unknown" strain protein peak files
  unknownProteinPeakFiles <- list.files(paste0(idbacPath, "\\Peak_Lists"), full.names = TRUE, pattern = "ProteinPeaks.rds")


  # connect to user-specified database
  dbcon <- DBI::dbConnect(RSQLite::SQLite(), databasePath)

  # Connect dplyr to database
  db <- dplyr::tbl(dbcon, "IDBacDatabase")



  unknownStrainIDs <- unlist(strsplit(basename(unknownProteinPeakFiles), "_ProteinPeaks.rds"))






  # Filter database by whatever metadata
  # Return only rds column and strain ID
  libSpec <- db %>%
    #filter(Strain_ID == "114A-2") %>%
    dplyr::select(c(Strain_ID,proteinPeaksRDS))




  # Note: cannot "slice()" or select by index within SQL table, so will just
  # filter by strain ID
  libStrainIDs <- libSpec %>%
    select(Strain_ID) %>%
    collect() %>%
      unlist() %>%
    as.vector()






  # Run the library search. Inputs: "unknownStrainIDs" ; "searchLibrary" = function
  # Returns list of top hit matches. Each list element = 1 unknown sample, with 1 best cosine similarity score = lib sample ID
  allScores <- lapply(unknownProteinPeakFiles, function(x) searchLibrary(x))
#  allScores <- searchLibrary(unknownProteinPeakFiles)



  allScores



}
