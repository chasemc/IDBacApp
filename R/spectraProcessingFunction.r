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
  
  
  sampleID <- IDBacApp::cleanWSpace(sampleID)
  

# Create version and metadata SQL tables ----------------------------------

  
  #Doesn't do anything currently, but put here to help future-proof
  IDBacApp::sqlCreate_version(userDBCon = userDBCon)
  #----
  
  # If sample ID doesn't exist, create it in table
  # TODO: userprompt with option to change ID
  IDBacApp::createMetaSQL(sampleID = sampleID,
                          userDBCon = userDBCon)
  
  

# Create XML table --------------------------------------------------------

  
  
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
  

  mzML_con<<-mzML_con
  scanNumber<<-scanNumber
  userDBCon<<-userDBCon
  sampleID<<-sampleID
  XMLinfo<<-XMLinfo
  rawDataFilePath<<-rawDataFilePath
  smallRangeEnd <<-6000
  
  
  sqlDataFrame <- IDBacApp::sqlTableArchitecture(numberScans = scanNumber)
  
  spectraImport <- mzR::peaks(mzML_con)
  
  # If only one spectrum, make it a list
  if (class(spectraImport) == "matrix") {
    spectraImport <- list(spectraImport)
  } 
  
  
  # List of serialized mass vectors
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
    sqlDataFrame$IndividualSpectra$smallMoleculeSpectrumIntensity[smallIndex] <- IDBacApp::mzRpeakSerializer(spectraImport[smallIndex], 
                                                                                                 column = "intensity")
    # # List of hashes
    sqlDataFrame$IndividualSpectra$spectrumIntensityHash[smallIndex] <- unlist(
      lapply(sqlDataFrame$IndividualSpectra$smallMoleculeSpectrumIntensity[smallIndex],
             function(x){
               IDBacApp::hashR(x)
             })
    )

    
    
    peaks <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport[smallIndex])
    
    peaks <- IDBacApp::processSmallMolSpectra(peaks)
    
    
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksMass[smallIndex] <- lapply(peaks, function(x) x@mass)
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksMass[smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$smallMoleculePeaksMass[smallIndex], 
                                                                         function(x){
                                                                           IDBacApp::compress(IDBacApp::serial(x))
                                                                         })
    
    
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksIntensity[smallIndex] <- lapply(peaks, function(x) x@intensity)
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksIntensity[smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$smallMoleculePeaksIntensity[smallIndex], 
                                                                         function(x){
                                                                           IDBacApp::compress(IDBacApp::serial(x))
                                                                         })
    
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksSNR[smallIndex] <- lapply(peaks, function(x) x@snr)
    sqlDataFrame$IndividualSpectra$smallMoleculePeaksSNR[smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$smallMoleculePeaksSNR[smallIndex], 
                                                                   function(x){
                                                                     IDBacApp::compress(IDBacApp::serial(x))
                                                                   })
    
    
    
    
    
  }  
    
    # Protein Spectra ---------------------------------------------------------
    
   if (any(!smallIndex)) {
    
    #List of serialized intensity vectors 
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex] <- IDBacApp::mzRpeakSerializer(spectraImport[!smallIndex], 
                                                                                        column = "intensity")
    # # List of hashes
    sqlDataFrame$IndividualSpectra$spectrumIntensityHash[!smallIndex] <- unlist(
      lapply(sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex],
             function(x){
               IDBacApp::hashR(x)
             })
    )
    
    
    
    peaks <- IDBacApp::spectrumMatrixToMALDIqaunt(spectraImport[!smallIndex])
    peaks <- IDBacApp::processProteinSpectra(peaks)
    
    sqlDataFrame$IndividualSpectra$proteinPeaksMass[!smallIndex] <- lapply(peaks, function(x) x@mass)
    sqlDataFrame$IndividualSpectra$proteinPeaksMass[!smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksMass[!smallIndex], 
                                                                    function(x){
                                                                      IDBacApp::compress(IDBacApp::serial(x))
                                                                    })
    
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex] <- lapply(peaks, function(x) x@intensity)
    sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksIntensity[!smallIndex], 
                                                                   function(x){
                                                                    IDBacApp::compress(IDBacApp::serial(x))
                                                                   })
    
    sqlDataFrame$IndividualSpectra$proteinPeaksSNR[!smallIndex] <- lapply(peaks, function(x) x@snr)
    sqlDataFrame$IndividualSpectra$proteinPeaksSNR[!smallIndex] <- lapply(sqlDataFrame$IndividualSpectra$proteinPeaksSNR[!smallIndex], 
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

  
  if (!DBI::dbExistsTable(userDBCon, "massTable")) {
    
    sta <- RSQLite::dbSendStatement(userDBCon, sqlDataFrame$massTableSQL)
    RSQLite::dbClearResult(sta)
  } 
  # Write to SQL DB
  DBI::dbWriteTable(conn = userDBCon,
                    name = "massTable", # SQLite table to insert into
                    sqlDataFrame$massTable, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
  
 
  

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










