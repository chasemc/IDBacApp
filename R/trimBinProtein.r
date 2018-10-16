
trimBinProtein <- function(injectLibrary, # Library path
                           idsToInject,
                           addToLibraryDendroLabel,
                           spectraID,
                           lowerMassCutoff,
                           upperMassCutoff,
                           massTolerance,
                           sqlDB){

db <- dplyr::tbl(sqlDB, "IndividualSpectra")


if(length(injectLibrary) == 0){      # If library injection is not selected:

# get spectrumSHA and strain ids
  db %>%
    filter(proteinPeaksRDS != "NA") %>%
    select(spectrumSHA, Strain_ID) %>%
    collect %>%
    return(.) -> ids
# group spectrumSHA for lapply
  ids <- split(ids$spectrumSHA, ids$Strain_ID)

  all <- lapply(ids, function(x) IDBacApp::collapseProteinReplicates2(db = db,
                                                                               spectrumSHA = x))

alw<<-all
    # Trim masses to user-specified lower and upper bounds and return as the result of the function
  MALDIquant::trim(all,
                   c(lowerMassCutoff, upperMassCutoff))

}else{ # library injection is selected

  # Get protein peak file locations for the non-library spectra
  all <- list.files(paste0(spectraPath, "\\Peak_Lists"),full.names = TRUE, pattern = "ProteinPeaks.rds")
  # Read Protein peak files into R
  all <- unlist(sapply(all, readRDS))



  libProteinPeaks <- lapply(injectLibrary, function(libraries){
    # connect to user-specified database
    dbcon <- DBI::dbConnect(RSQLite::SQLite(), libraries)

    # Connect dplyr to database
    db <- dplyr::tbl(dbcon, "IDBacDatabase")

    # Return the SQL blob for the individual strain protein peaks MALDIquant object

    libProteinPeaks <-  db %>%
      dplyr::filter(Strain_ID %in% idsToInject) %>%
      dplyr::select(proteinPeaksRDS) %>%
      collect()
    libProteinPeaks <- as.list(libProteinPeaks)[[1]]

    # Get the protein peak "rds" file
    libProteinPeaks <-  lapply(libProteinPeaks, function(x){
      #Decompress blob
      libProteinPeaks <- memDecompress(x, type="gzip")
      # Unserialize blob
      libProteinPeaks <- unserialize(libProteinPeaks, NULL)
      # Unlist rds file list
      libProteinPeaks <- unlist(libProteinPeaks, recursive = TRUE)
    })


    libProteinPeaks <- unlist(libProteinPeaks, recursive = TRUE)
    oldID <- as.character(lapply(libProteinPeaks, function(x) x@metaData$Strain))

    meta <- db %>%
      dplyr::select(c("Strain_ID", addToLibraryDendroLabel)) %>%
      collect()

    rowIndex <- match(oldID, meta$Strain_ID)



    if(length(libProteinPeaks) > 0){

      for(i in 1:length(libProteinPeaks)){

        # Vector of selected metaData
        # as.character(meta[rowIndex[[i]], -1])
        libProteinPeaks[[i]]@metaData$Strain <- paste0("Library Hit: ", libProteinPeaks[[i]]@metaData$Strain, "_", paste0(as.character(meta[rowIndex[[i]], -1]), collapse = "_"))

      }}

    libProteinPeaks
  })

  all <- c(all,  unlist(libProteinPeaks, recursive = TRUE))




  # Check if protein spectra exist
  shiny::validate(
    shiny::need(!is.null(all),"The hierarchical clustering and PCA analyses require you to first visit the \"Compare Two Samples (Protein)\"
         tab at the top of the page.")
    )
  # Bin protein peaks
  all <- MALDIquant::binPeaks(all, tolerance = massTolerance, method="relaxed")
  MALDIquant::trim(all, c(lowerMassCutoff, upperMassCutoff))

}

}
