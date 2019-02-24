#' Create IDBac SQLite database 
#' Should work for mzML, mzXML, mgf, and txt data input
#' 
#'  
#' @param rawDataFilePath filepath of the data
#' @param sampleID the sample ID to be read and added to the database
#' @param userDBCon database connection (checked out pool)
#'
#' @return the peak list modifed by binning then subtractng the matrix sample,
#' @export
spectraProcessingFunction <- function(rawDataFilePath,
                                      sampleID,
                                      userDBCon){
  
  
  
  #Doesn't do anything currently, but put here to help future-proof
  
  if(!"version" %in%  DBI::dbListTables(userDBCon)){
    
    # Add version table
    DBI::dbWriteTable(conn = userDBCon,
                      name = "version", # SQLite table to insert into
                      IDBacApp::sqlTableArchitecture(numberScans = 1)$version, # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
  }
  #----
  
  # If sample ID doesn't exist, create it in table
  # TODO: userprompt with option to change ID
  IDBacApp::createMetaSQL(sampleID = sampleID,
                          userDBCon = userDBCon)
  
  # Make connection to mzML file
  mzML_con <- mzR::openMSfile(rawDataFilePath,
                              backend = "pwiz")
  
  XMLinfo <- IDBacApp::createXMLSQL(sampleID = sampleID,
                                    rawDataFilePath = rawDataFilePath,
                                    userDBCon = userDBCon,
                                    mzML_con = mzML_con)
  
  
  # Get number of spectra contained in mzML
  scanNumber <- nrow(mzR::header(mzML_con))
  
  # Loop over each spectrum inside an mzmL file
  
  IDBacApp::createSpectraSQL(mzML_con = mzML_con,
                             scanNumber = scanNumber,
                             userDBCon = userDBCon,
                             sampleID = sampleID,
                             XMLinfo = XMLinfo, 
                             rawDataFilePath = rawDataFilePath)
  
}


#' createSpectraSQL
#'
#' @param mzML_con NA
#' @param scanNumber NA
#' @param userDBCon NA
#' @param sampleID NA
#' @param XMLinfo NA
#' @param rawDataFilePath NA
#' @param smallRangeEnd end of mass region for small mol, if m/z above this- will be classified as "protein" spectrum
#'
#' @return NA
#' @export
#'

createSpectraSQL <- function(mzML_con, 
                             scanNumber,
                             userDBCon,
                             sampleID,
                             XMLinfo,
                             rawDataFilePath,
                             smallRangeEnd = 6000){
  

  
  
  sqlDataFrame <- IDBacApp::sqlTableArchitecture(numberScans = scanNumber)
  
  spectraImport <- mzR::peaks(mzML_con)
  
  #List of serialized mass vectors
  sqlDataFrame$massTable$binaryMassVector <- IDBacApp::mzRpeakSerializer(spectraImport, column = "mass")
  # List of hashes
  sqlDataFrame$massTable$spectrumMassHash <- unlist(lapply(sqlDataFrame$massTable$binaryMassVector,
                                                           function(x) {
                                                             IDBacApp::hashR(x)
                                                           })
  )
  sqlDataFrame$IndividualSpectra$spectrumMassHash <- sqlDataFrame$massTable$spectrumMassHash
  
  
  # get maximum masses of mass vectors. True = small mol, False = protein
  smallIndex <- unlist(lapply(spectraImport, function(x) max(x[,1])))
  smallIndex <- smallIndex < smallRangeEnd
  
  # Small mol spectra -------------------------------------------------------
  
  if (any(smallIndex)) { 
    
    #List of serialized intensity vectors 
    sqlDataFrame$IndividualSpectra$smallMoleculeSpectrumIntensity <- IDBacApp::mzRpeakSerializer(spectraImport[smallIndex], 
                                                                                                 column = "intensity")
    # # List of hashes
    sqlDataFrame$IndividualSpectra$spectrumIntensityHash <- unlist(
      lapply(sqlDataFrame$IndividualSpectra$smallMoleculeSpectrumIntensity,
             function(x){
               IDBacApp::hashR(x)
             })
    )

    
    
    peaks <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport[smallIndex])
    
    peaks <- IDBacApp::processSmallMolSpectra(peaks)
    
    
   
    
    
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksIntensity <- lapply(peaks, function(x) x@intensity)
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksIntensity <- lapply(sqlDataFrame$IndividualSpectra$smallMoleculePeaksIntensity, 
                                                                         function(x){
                                                                           IDBacApp::compress(IDBacApp::serial(x))
                                                                         })
    
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksSNR <- lapply(peaks, function(x) x@snr)
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksSNR <- lapply(sqlDataFrame$IndividualSpectra$smallMoleculePeaksSNR, 
                                                                   function(x){
                                                                     IDBacApp::compress(IDBacApp::serial(x))
                                                                   })
    
    
    
    
    
  }  
    
    # Protein Spectra ---------------------------------------------------------
    
   if (any(!smallIndex)) {
    
    #List of serialized intensity vectors 
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity <- IDBacApp::mzRpeakSerializer(spectraImport[!smallIndex], 
                                                                                        column = "intensity")
    # # List of hashes
    sqlDataFrame$IndividualSpectra$spectrumIntensityHash <- unlist(
      lapply(sqlDataFrame$IndividualSpectra$proteinPeaksIntensity,
             function(x){
               IDBacApp::hashR(x)
             })
    )
    
    
    
    peaks <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport[!smallIndex])
    peaks <- IDBacApp::processProteinSpectra(peaks)
    
 
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity <- lapply(peaks, function(x) x@intensity)
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksIntensity, 
                                                                   function(x){
                                                                    IDBacApp::compress(IDBacApp::serial(x))
                                                                   })
    
    sqlDataFrame$IndividualSpectra$proteinPeaksSNR <- lapply(peaks, function(x) x@snr)
    sqlDataFrame$IndividualSpectra$proteinPeaksSNR <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksSNR, 
                                                             function(x){
                                                              IDBacApp::compress(IDBacApp::serial(x))
                                                             })
    
    
    
  }
  
  remove(peaks)
  
  
  
  sqlDataFrame$IndividualSpectra$mzMLHash <- XMLinfo$mzMLHash
  sqlDataFrame$IndividualSpectra$Strain_ID <- sampleID
  
  if ("MassError" %in% ls(XMLinfo$mzMLInfo)) {
    sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[individualSpectrum]]
  }
  sqlDataFrame$IndividualSpectra$AcquisitionDate <- XMLinfo$mzMLInfo$AcquisitionDate
  


# Write massTable ---------------------------------------------------------

  
  if (!DBI::dbExistsTable(userDBCon, "IndividualSpectra")) {
    
    sta <- RSQLite::dbSendStatement(userDBCon, sqlDataFrame$massTableSQL)
    RSQLite::dbClearResult(sta)
  } 
  
  massTable
  
 
  

# Write IndividualSpectra -------------------------------------------------

  
  
  
  if (!DBI::dbExistsTable(userDBCon, "IndividualSpectra")) {
    
    sta <- RSQLite::dbSendStatement(userDBCon, sqlDataFrame$IndividualSpectraSQL)
    RSQLite::dbClearResult(sta)
  }
  # Write to SQL DB
  DBI::dbWriteTable(conn = userDBCon,
                    name = "IndividualSpectra", # SQLite table to insert into
                    sqlDataFrame$IndividualSpectra, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
  
  
  
  
}








#' createMetaSQL
#'
#' @param sampleID NA
#' @param userDBCon NA
#'
#' @return NA
#' @export
#'

createMetaSQL <- function(sampleID,
                          userDBCon){
  
  # If metaData table already exists, prevent adding a duplicate entry
  if ("metaData" %in% DBI::dbListTables(userDBCon)) {
    
    existingMeta <- glue::glue_sql("SELECT `Strain_ID`
                                 FROM `metaData`",
                                   .con = userDBCon)
    
    existingMeta <- DBI::dbGetQuery(conn = userDBCon,
                                    statement = existingMeta)
    
    
    
    
    if (sampleID %in% existingMeta){
      warning(base::paste0("The sample ID \"", sampleID, "\" already exists in \"", basename(userDBCon@dbname), "\", not adding again."))
    } else {
      # Generate base SQL table
      sqlDataFrame <- IDBacApp::sqlTableArchitecture(numberScans = 1)
      
      sqlDataFrame$metaData$Strain_ID <- sampleID
      
      # Write to SQL DB  (There is no sample level metadata to add at this point)
      DBI::dbWriteTable(conn = userDBCon,
                        name = "metaData", # SQLite table to insert into
                        sqlDataFrame$metaData, # Insert single row into DB
                        append = TRUE, # Append to existing table
                        overwrite = FALSE) # Do not overwrite
      
    }
  } else {
    # Generate base SQL table
    sqlDataFrame <- IDBacApp::sqlTableArchitecture(numberScans = 1)
    
    sqlDataFrame$metaData$Strain_ID <- sampleID
    
    # Write to SQL DB  (There is no sample level metadata to add at this point)
    DBI::dbWriteTable(conn = userDBCon,
                      name = "metaData", # SQLite table to insert into
                      sqlDataFrame$metaData, # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
    
  }
  
}


#' createXMLSQL
#'
#' @param rawDataFilePath NA
#' @param sampleID NA
#' @param userDBCon NA
#' @param mzML_con NA
#'
#' @return NA
#' @export
#'

createXMLSQL <- function(rawDataFilePath,
                         sampleID,
                         userDBCon,
                         mzML_con){
  
  sqlDataFrame <- IDBacApp::sqlTableArchitecture(numberScans = 1)
  
  # Read mzML file and create a hash
  
  sqlDataFrame$XML$XML <- list(a = base::readLines(rawDataFilePath))
  
  # XML file doeesn't get compressed because not much better compression with gzip after serialization,
  # at least for mzML files generated using IDBac's msconvert settings
  sqlDataFrame$XML$XML <- list(IDBacApp::serial(sqlDataFrame$XML$XML))
  
 
  sqlDataFrame$XML$mzMLHash <- IDBacApp::hashR(sqlDataFrame$XML$XML[[1]])
  
  
  if ("XML" %in% DBI::dbListTables(userDBCon)) {
    
    existingSHA <- glue::glue_sql("SELECT `mzMLHash`
                                 FROM `XML`",
                                  .con = userDBCon)
    
    existingSHA <- DBI::dbGetQuery(conn = userDBCon,
                                   statement = existingSHA)
  } else {
    existingSHA <- matrix(NA)
  }
  
  
  # Generate base SQL table
  
  #------- Create SQL "XML" table entry
  
  
  # Get instrument Info
  instInfo <- mzR::instrumentInfo(mzML_con)
  instInfo <- as.data.frame(instInfo, 
                            stringsAsFactors = F)
  
  sqlDataFrame$XML$manufacturer  <- instInfo$manufacturer
  sqlDataFrame$XML$model         <- instInfo$model
  sqlDataFrame$XML$ionisation    <- instInfo$ionisation
  sqlDataFrame$XML$analyzer      <- instInfo$analyzer
  sqlDataFrame$XML$detector      <- instInfo$detector
  
  # Find acquisitonInfo from mzML file
  acquisitonInfo <- IDBacApp::findAcquisitionInfo(rawDataFilePath,
                                                  instInfo$manufacturer)
  
  if ("Instrument_MetaFile" %in% ls(acquisitonInfo)) { 
    sqlDataFrame$XML$Instrument_MetaFile <- IDBacApp::serial(acquisitonInfo$Instrument_MetaFile)
  }
  
  if (sqlDataFrame$XML$mzMLHash %in% existingSHA[,1]) {
    warning("A mzML file matching \"", sampleID, "\" already seems to be present, file and contents not added again.")
  } else {
    # Write to SQL DB
    DBI::dbWriteTable(conn = userDBCon,
                      name = "XML", # SQLite table to insert into
                      sqlDataFrame$XML, # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
  }
  
  return(list(mzMLHash = sqlDataFrame$XML$mzMLHash,
              mzMLInfo = acquisitonInfo))
}

