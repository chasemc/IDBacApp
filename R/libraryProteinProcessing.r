libraryProteinProcessing <-  function(singleLibSpec, dbConnection){


  # "singleLibSpec" is a character vector of sample IDs.  hese should correspond to the Entries in the "Strain_ID" column of the SQLite database
  # "dbConnection" is a DBI connection to an SQLite database

  # "libraryProteinProcessing" finds the matching sample ID in the "Strain_ID" column
if(length(singleLibSpec) > 1){
  warning("More than one Strain_ID passed to libraryProteinProcessing.r")
}
  # Return the "proteinPeaksRDS" SQL blob for the individual strain. w/parameterized query
  sqlQuery <- glue::glue_sql("SELECT proteinPeaksRDS FROM {`sqlTable`} WHERE Strain_ID IN ({vals*})",
                             vals = singleLibSpec, sqlTable="IDBacDatabase", .con = dbcon)

  sqlQuery <- dbSendQuery(dbcon, sqlQuery)
  proteinBlobs <- dbFetch(sqlQuery)
  dbClearResult(sqlQuery)
  proteinBlobs <- unlist(proteinBlobs, recursive = FALSE)


unlist(lapply(proteinBlobs, function(libProteinPeaks){
  # dplyr equivalent
  # Connect dplyr to database
  # dbConnection <- dplyr::tbl(dbConnection, "IDBacDatabase")
  # libProteinPeaks <-  dbConnection %>%
  #   filter(Strain_ID %in% singleLibSpec) %>%
  #   select(proteinPeaksRDS) %>%
  #   collect()

  #Decompress blob
  libProteinPeaks <- memDecompress(libProteinPeaks, type="gzip")
  # Unserialize blob
  libProteinPeaks <- unserialize(libProteinPeaks, NULL)
  # Unlist proteinPeaksRDS file list to make sure it wasn't saved as list of list
  libProteinPeaks <- unlist(libProteinPeaks, recursive = TRUE)

  # Get sample IDs contained in the metadata *within* the rds MALDIquant protein peaks object
  labs <- sapply(libProteinPeaks, function(x)metaData(x)$Strain)
  # Change to facter
  labs <- factor(labs)
  # Setup for-loop
  new2 <- NULL
  newPeaks <- NULL

  # If libProteinPeaks contains more than one sample ID (it shouldn't), make sure to bin and
  # filter peaks only within the sample sample
  for (i in seq_along(levels(labs))) {
    specSubset <- (which(labs == levels(labs)[[i]]))
    if (length(specSubset) > 1) { # ie if there is > 1 protein spectrum (replicate)
      libProteinPeaks <- MALDIquant::trim(libProteinPeaks, c(3000,15000))
      # See here for info on "tolerance" value
      # https://github.com/sgibb/MALDIquant/issues/56#issuecomment-388133351
      libProteinPeaks <- MALDIquant::binPeaks(libProteinPeaks, tolerance = .002, method = "relaxed")
      libProteinPeaks <- mergeMassPeaks(libProteinPeaks, method="sum")
      new2 <- c(new2, libProteinPeaks)
    } else{ # If there is only one spectrum
      new2 <- c(new2, libProteinPeaks[specSubset])
    }

  }
  new2
}))
}
