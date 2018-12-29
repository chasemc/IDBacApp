#' Create IDBac SQLite database 
#' Should work for mzML, mzXML, mgf, and txt data input
#' 
#'  
#' @param rawDataFilePath filepath of the data
#' @param sampleID the sample ID to be read and added to the database
#' @param userDBCon database connection (checked out pool)

#' @return the peak list modifed by binning then subtractng the matrix sample,
#' or just the binned peak list if no matrix wsa provided


# -----------------
spectraProcessingFunction <- function(rawDataFilePath,
                                      sampleID,
                                      userDBCon){
  aa1<<-rawDataFilePath
  aa2<<-sampleID
  aa3<<-userDBCon
  
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
  
  # Lapply over each spectrum inside an mzmL file
  wq <- lapply(1:scanNumber, 
               function(individualSpectrum){
                 
                 sqlDataFrame <- IDBacApp::sqlTableArchitecture(nrow = 1)
                 
                 sqlDataFrame$IndividualSpectra$mzMLSHA <- XMLinfo
                 sqlDataFrame$IndividualSpectra$Strain_ID <- sampleID
                 sqlDataFrame$IndividualSpectra$spectrumSHA <- individualRawSpecSHA$spectrumSHA[[individualSpectrum]]
                 sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[individualSpectrum]]
                 sqlDataFrame$IndividualSpectra$AcquisitionDate <- acquisitonInfo$AcquisitionDate[[individualSpectrum]]
                 
                 
                 
                 
                 spectraImport <- mzR::peaks(mzML_con, scans = individualSpectrum)
                 
                 if(typeof(spectraImport) == "list"){
                   
                   spectraImport <- lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                                                                     intensity = x[ , 2],
                                                                                                     metaData = list(File = acquisitonInfo$rawFilePaths[[individualSpectrum]],
                                                                                                                     Strain = ids)))
                 } else if(typeof(spectraImport) == "double") {
                   
                   spectraImport <- MALDIquant::createMassSpectrum(mass = spectraImport[ , 1],
                                                                   intensity = spectraImport[ , 2],
                                                                   metaData = list(File = acquisitonInfo$rawFilePaths[[individualSpectrum]],
                                                                                   Strain = ids))
                 }
                 
                 
                 # Make sure to return spectraImport as a MassSpectrumList
                 if(MALDIquant::isMassSpectrumList(spectraImport)){
                   
                 }else if(MALDIquant::isMassSpectrum(spectraImport)){
                   spectraImport <- list(spectraImport)
                 }
                 
                 
                 
                 
                 if(max(MALDIquant::mass(spectraImport[[1]])) > 5000){ # if it's a protein spectrum
                   
                   sqlDataFrame$IndividualSpectra$proteinSpectrum <- IDBacApp::compress(IDBacApp::serial(spectraImport))
                   
                   
                   
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
                   sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum <- IDBacApp::serial(spectraImport) 
                   sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum <- IDBacApp::compress(sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum)
                   sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum <- list(sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum)
                   
                   
                   
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
                 a <- colnames(IDBacApp::sqlTableArchitecture(nrow = 1)$IndividualSpectra)
                 b <- colnames(sqlDataFrame$IndividualSpectra)
                 
                 for(i in a[!a %in% b]){
                   sqlDataFrame$IndividualSpectra[,i] <- NA
                 }
                 
                 
                 return(sqlDataFrame$IndividualSpectra[1, a])
                 
                 
                 
               })
  
  
  
  
  wqq <- unlist(wq, recursive = FALSE)
  
  
  for(i in base::seq_along(wqq)){
    
    # Write to SQL DB
    DBI::dbWriteTable(conn = conn,
                      name = "IndividualSpectra", # SQLite table to insert into
                      wqq[[i]], # Insert single row into DB
                      append = TRUE, # Append to existing table
                      overwrite = FALSE) # Do not overwrite
  }
  pool::poolReturn(conn)
  
  
  
}









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
  
  QQ<<-(list(mzMLSHA = sqlDataFrame$XML$mzMLSHA,
              mzMLInfo = acquisitonInfo))
}


findAcquisitionInfo <- function(rawFilepaths,
                                manufacturer){
  
  if(!exists("sha")){
    sha <- new.env(parent = parent.frame())
  }
  
  files <-  gsub("file://", "", rawFilepaths)
  files <- dirname(files)
  
  #gat Bruker flex series metadata:
  
  try({
    
    if(manufacturer == "Bruker Daltonics flex series"){
      files <- files[which(file.exists(files))]
      if(length(files) > 0){
        
        files <- list.files(files, pattern="acqus", recursive = TRUE, full.names = TRUE)
        sha$Instrument_MetaFile  <- lapply(files, function(x)  read.delim(x, sep="\n")) # Find Acqu file
        sha$MassError <- unlist(lapply(sha$Instrument_MetaFile , function(x) as.character(x[grep("Masserr", x[,1]),]))) #Parse the Acqu file for the mass error row
        sha$MassError <- unlist(lapply(sha$MassError, function(x) as.numeric(strsplit(x, "##\\$Masserr= " )[[1]][[2]])))
        sha$AcquisitionDate <- unlist(lapply(sha$Instrument_MetaFile , function(x) as.character(x[grep("##\\$AQ_DATE", x[,1]),]))) #Parse the Acqu file for the mass error row
        sha$AcquisitionDate <- unlist(lapply(sha$AcquisitionDate, function(x) gsub('^.*<\\s*|\\s*.>.*$', '', x)))
      }
    } else {
      sha$AcquisitionDate <-  base::Sys.Date() 

    }
    
  })
  
  return(sha)
}


