# Contains the following functions:
# processProteinLibSpec
# libraryProteinProcessing
# unknownProcessing



processProteinLibSpec <- function(spectra){

  # "spectra" = list of lists of MALDIquant peak objects
  # > list(b)
  # [[1]]
  # [[1]][[1]]
  # S4 class type            : MassPeaks
  # Number of m/z values     : 144
  # Range of m/z values      : 1919.939 - 10444.253
  # Range of intensity values: 2.682e+00 - 1.082e+02
  # Range of snr values      : 4.032 - 162.6
  # Memory usage             : 5.031 KiB
  #
  # [[1]][[2]]
  # S4 class type            : MassPeaks
  # Number of m/z values     : 133
  # Range of m/z values      : 1919.939 - 9506.526
  # Range of intensity values: 2.541e+00 - 7.357e+01
  # Range of snr values      : 4.039 - 116.964
  # Memory usage             : 4.773 KiB


  # If libProteinPeaks contains more than one sample ID (it shouldn't), make sure to bin and
  # filter peaks only within the sample sample

  if (length(spectra) > 1) { # ie if there is > 1 protein spectrum (replicate)
    libProteinPeaks <- MALDIquant::trim(spectra, c(3000,15000))
    # See here for info on "tolerance" value
    # https://github.com/sgibb/MALDIquant/issues/56#issuecomment-388133351
    libProteinPeaks <- MALDIquant::binPeaks(libProteinPeaks, tolerance = .002, method = "relaxed")
    libProteinPeaks <- mergeMassPeaks(libProteinPeaks, method="sum")
    return(unname(libProteinPeaks))
  }else{ # If there is only one spectrum
    libProteinPeaks <- MALDIquant::trim(spectra, c(3000,15000))
    return(unname(libProteinPeaks))
  }

}



getLibraryProteinSpectrum <-  function(singleLibSpec, dbConnection){


  # "singleLibSpec" is a character vector of mzXML hex.
  # hese should correspond to the Entries in the "Strain_ID" column of the SQLite database
  # "dbConnection" is a DBI connection to an SQLite database

  # "getLibraryProteinSpectrum" finds the matching sample ID in the "Strain_ID" column
  if(length(singleLibSpec) > 1){
    warning("More than one Strain_ID passed to getLibraryProteinSpectrum")
  }
  # Return the "proteinPeaksRDS" SQL blob for the individual strain. w/parameterized query
  sqlQuery <- glue::glue_sql("SELECT proteinPeaksRDS FROM {`sqlTable`} WHERE Strain_ID IN ({vals*})",
                             vals = singleLibSpec, sqlTable="IDBacDatabase", .con = dbConnection)

  sqlQuery <- dbSendQuery(dbConnection, sqlQuery)
  proteinBlobs <- dbFetch(sqlQuery)
  dbClearResult(sqlQuery)
  proteinBlobs <- unlist(proteinBlobs, recursive = FALSE)


  aqq1 <- unlist(lapply(proteinBlobs, function(libProteinPeaks){
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
    (unlist(libProteinPeaks, recursive = TRUE))

  }))
  q2<<-aqq1
  processProteinLibSpec(aqq1)

}



unknownProcessing <-  function(rdsPath){


  unknownSpectrum <- unlist(lapply(rdsPath, readRDS), recursive = TRUE)

  labs <- sapply(unknownSpectrum, function(x)metaData(x)$Strain)

  # if(length(labs) > 1){
  #       if(identical(labs)){
  #        return(processProteinLibSpec())
  #     }
  #     else{
  #       warning("In \"unknownProcessing\" function: ", all.equal(labs))
  #     }
  # }
  q1<<-unknownSpectrum
  z1 <<-rdsPath
  return(processProteinLibSpec((unknownSpectrum)))


}


