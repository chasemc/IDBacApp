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
  
  # Write to SQL DB
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
#'
#' @return NA
#' @export
#'

createSpectraSQL <- function(mzML_con, 
                             scanNumber,
                             userDBCon,
                             sampleID,
                             XMLinfo,
                             rawDataFilePath){
  
  for (individualSpectrum in 1:scanNumber){
    
    sqlDataFrame <- IDBacApp::sqlTableArchitecture(numberScans = 1)
   
    spectraImport <- mzR::peaks(mzML_con, 
                                scans = individualSpectrum)
    sqlDataFrame$IndividualSpectra$spectrumSHA <- IDBacApp::createMZsha(spectraImport)
    
     
    if("IndividualSpectra" %in% DBI::dbListTables(userDBCon)){
      
      existingSHA <- glue::glue_sql("SELECT `spectrumSHA`
                                    FROM `IndividualSpectra`",
                                    .con = userDBCon)
      
      existingSHA <- DBI::dbGetQuery(conn = userDBCon,
                                     statement = existingSHA)
      
    } else {
      existingSHA <- matrix(NA)
    }
    
    if(sqlDataFrame$IndividualSpectra$spectrumSHA %in% existingSHA[,1]) {
      warning ("One of the spectra for \"", sampleID, "\" already seems to be present, spectrum not added again.")
    } else {
    
    sqlDataFrame$IndividualSpectra$mzMLSHA <- XMLinfo$mzMLSHA
    sqlDataFrame$IndividualSpectra$Strain_ID <- sampleID
    
    if("MassError" %in% ls(XMLinfo$mzMLInfo)){
      sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[individualSpectrum]]
    }
    sqlDataFrame$IndividualSpectra$AcquisitionDate <- XMLinfo$mzMLInfo$AcquisitionDate
    
    
    
    
    if(typeof(spectraImport) == "list"){
      
      spectraImport <- lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                                                        intensity = x[ , 2],
                                                                                        metaData = list(File = rawDataFilePath,
                                                                                                        Strain = sampleID)))
    } else if(typeof(spectraImport) == "double") {
      
      spectraImport <- MALDIquant::createMassSpectrum(mass = spectraImport[ , 1],
                                                      intensity = spectraImport[ , 2],
                                                      metaData = list(File = rawDataFilePath,
                                                                      Strain = sampleID))
    }
    
    
    # Make sure to return spectraImport as a MassSpectrumList
    if(MALDIquant::isMassSpectrumList(spectraImport)){
      
    }else if(MALDIquant::isMassSpectrum(spectraImport)){
      spectraImport <- list(spectraImport)
    }
    
    
    
    
    if(max(MALDIquant::mass(spectraImport[[1]])) > 5000){ # if it's a protein spectrum
      
      sqlDataFrame$IndividualSpectra$proteinSpectrum <- list(IDBacApp::compress(IDBacApp::serial(spectraImport)))
      
      
      
      # Why square root transformation and not log:
      #  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
      # Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
      #"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
      #because for all three intensity groups in Table 1, the variance is approximately constant."
      
      
      spectraImport <- MALDIquant::transformIntensity(spectraImport, 
                                                      method = "sqrt") 
      spectraImport <- MALDIquant::smoothIntensity(spectraImport,
                                                   method = "SavitzkyGolay", 
                                                   halfWindowSize = 20) 
      spectraImport <- MALDIquant::removeBaseline(spectraImport,
                                                  method = "TopHat") 
      spectraImport <- MALDIquant::calibrateIntensity(spectraImport,
                                                      method="TIC")  
      spectraImport <- MALDIquant::detectPeaks(spectraImport, 
                                               method = "MAD", 
                                               halfWindowSize = 20, 
                                               SNR = 3)
      spectraImport <- IDBacApp::serial(spectraImport)
      spectraImport <- IDBacApp::compress(spectraImport)
      sqlDataFrame$IndividualSpectra$proteinPeaks <- list(spectraImport)
      
      
    }else{
      ############
      #Spectra Preprocessing, Peak Picking
      
      sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum <- list(IDBacApp::compress(IDBacApp::serial(spectraImport)))
      
      
      spectraImport <- MALDIquant::smoothIntensity(spectraImport,
                                                   method = "SavitzkyGolay",
                                                   halfWindowSize = 20) 
      spectraImport <- MALDIquant::removeBaseline(spectraImport,
                                                  method = "TopHat")
      spectraImport <- MALDIquant::calibrateIntensity(spectraImport, 
                                                      method="TIC") 
      #Find all peaks with SNR >1, this will allow us to filter by SNR later, doesn't effect the peak-picking algorithm, just makes files bigger
      spectraImport <- MALDIquant::detectPeaks(spectraImport, 
                                               method = "SuperSmoother",
                                               halfWindowSize = 20, 
                                               SNR = 1)
      spectraImport <- IDBacApp::serial(spectraImport)
      spectraImport <- IDBacApp::compress(spectraImport)
      sqlDataFrame$IndividualSpectra$smallMoleculePeaks <- list(spectraImport)
      
    }
    a <- colnames(IDBacApp::sqlTableArchitecture(numberScans = 1)$IndividualSpectra)
    b <- colnames(sqlDataFrame$IndividualSpectra)
    
    for(i in a[!a %in% b]){
      sqlDataFrame$IndividualSpectra[,i] <- NA
    }
    
    
    
    
    
    
    
    # Write to SQL DB
    DBI::dbWriteTable(conn = userDBCon,
                      name = "IndividualSpectra", # SQLite table to insert into
                      sqlDataFrame$IndividualSpectra[1, a], # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
  }
}

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
  if("metaData" %in% DBI::dbListTables(userDBCon)){
    
    existingMeta <- glue::glue_sql("SELECT `Strain_ID`
                                 FROM `metaData`",
                                   .con = userDBCon)
    
    existingMeta <- DBI::dbGetQuery(conn = userDBCon,
                                    statement = existingMeta)
    
    
    
    
    if(sampleID %in% existingMeta){
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
  
  sqlDataFrame$XML$mzMLSHA <- IDBacApp::hashR(sqlDataFrame$XML$XML)
  
  
  if("XML" %in% DBI::dbListTables(userDBCon)){
    
    existingSHA <- glue::glue_sql("SELECT `mzMLSHA`
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
  
  if("Instrument_MetaFile" %in% ls(acquisitonInfo)){ 
    sqlDataFrame$XML$Instrument_MetaFile <- IDBacApp::serial(acquisitonInfo$Instrument_MetaFile)
  }
  
  if (sqlDataFrame$XML$mzMLSHA %in% existingSHA[,1]) {
    warning ("A mzML file matching \"", sampleID, "\" already seems to be present, file and contents not added again.")
  } else {
    # Write to SQL DB
    DBI::dbWriteTable(conn = userDBCon,
                      name = "XML", # SQLite table to insert into
                      sqlDataFrame$XML, # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
  }
  
  return(list(mzMLSHA = sqlDataFrame$XML$mzMLSHA,
             mzMLInfo = acquisitonInfo))
}

