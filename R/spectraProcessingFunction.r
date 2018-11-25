# -----------------
spectraProcessingFunction <- function(rawDataFilePath, sample_ID, userDBCon){


# Number of mzML files
  n <- length(rawDataFilePath)
  
  # Generate base SQL table
  sqlDataFrame <- IDBacApp::sqlTableArchitecture(nrow= n)
  
  
  
#------- Metadata Table
 
  # Vector of sample names
  sqlDataFrame$metaData$Strain_ID <- sample_ID
 
  ids <- sqlDataFrame$metaData$Strain_ID
  
  
  conn <- pool::poolCheckout(userDBCon)
  # Write to SQL DB  (There is no sample level metadata to add at this point)
  DBI::dbWriteTable(conn = conn,
                    name = "metaData", # SQLite table to insert into
                    sqlDataFrame$metaData, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite

  pool::poolReturn(conn)
  

#------- Create SQL "XML" table entry


  # Make list of connections to mzML files, don't read (memory pointer)
  mzML_con <- lapply(rawDataFilePath, function(x) mzR::openMSfile(x, backend = "pwiz"))
  
  # Get instrument Info
  instInfo <- lapply(mzML_con, mzR::instrumentInfo)
  instInfo <- do.call(rbind.data.frame, c(instInfo, stringsAsFactors = F))
  
  sqlDataFrame$XML$manufacturer  <- instInfo$manufacturer
  sqlDataFrame$XML$model         <- instInfo$model
  sqlDataFrame$XML$ionisation    <- instInfo$ionisation
  sqlDataFrame$XML$analyzer      <- instInfo$analyzer
  sqlDataFrame$XML$detector      <- instInfo$detector

  remove(instInfo)


  sqlDataFrame$XML$XML <- lapply(rawDataFilePath, readLines)
  sqlDataFrame$XML$mzMLSHA <- unlist(lapply(sqlDataFrame$XML$XML, digest::sha1))
  # XML file doeesn't get compressed because not much better compression with gzip after serialization
  sqlDataFrame$XML$XML  <- lapply(sqlDataFrame$XML$XML, function(x) serialize(x, NULL))
 
  
  mzMLSHA1 <-sqlDataFrame$XML$mzMLSHA

  # Find individual spectra SHA and raw data filepath from mzML
  individualRawSpecSHA <- lapply(rawDataFilePath, IDBacApp::findRawSHAandFile)
  
  
  # Find acquisitonInfo from mzML file
  acquisitonInfo <- IDBacApp::findAcquisitionInfo(individualRawSpecSHA$rawDataFilePath,
                                                  individualRawSpecSHA$manufacturer)

  if(length(acquisitonInfo) > 0){ 
    
  # Get acqus file as list of blobs
  acquisitonInfo$Instrument_MetaFile %>%
    serialize(object = .,
              connection = NULL,
              ascii = FALSE,
              xdr = FALSE,
              version = 3) %>%
    list(.) -> sqlDataFrame$XML$Instrument_MetaFile
}
  conn <- pool::poolCheckout(userDBCon)

  
  
  # Write to SQL DB
  DBI::dbWriteTable(conn = conn,
                    name = "XML", # SQLite table to insert into
                    sqlDataFrame$XML, # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite

  
  pool::poolReturn(conn)
 
  


  #----------------------------------------------
  #----------------------------------------------
  # Create SQL "IndividualSpectra" entry

  #----------------------------------------------
  # Assign information about the acquisition



  sqlDataFrame$XML <-NULL # Free up memory


  check <- length(acquisitonInfo$Instrument_MetaFile) == length(acquisitonInfo$AcquisitionDate) &&
    length(acquisitonInfo$Instrument_MetaFile) == length(acquisitonInfo$MassError)



  if(check == FALSE){
    warning("spectraProcessingFunction.r: Acquisiton information elements are of different length")
  }

  
  #------------------------------














# If there's only one mzml file then we need to turn into a list for lapply
if(typeof(mzML_con) == "S4"){
  mzML_con <- list(mzML_con)
}

  

 wq <- lapply(base::seq_along(mzML_con),
              function(yeah){
                # Lapply over each spectrum inside an mzmL file
                lapply(1:nrow(mzR::header(mzML_con[[yeah]])), 
                       function(individualSpectrum){

    # Reset
    sqlDataFrame <- IDBacApp::sqlTableArchitecture(nrow = 1)

        sqlDataFrame$IndividualSpectra$mzMLSHA <- mzMLSHA1[[yeah]]
    sqlDataFrame$IndividualSpectra$Strain_ID <- ids[[yeah]]
    sqlDataFrame$IndividualSpectra$spectrumSHA <- individualRawSpecSHA[[yeah]]$spectrumSHA[[individualSpectrum]]
    sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[individualSpectrum]]
    sqlDataFrame$IndividualSpectra$AcquisitionDate <- acquisitonInfo$AcquisitionDate[[individualSpectrum]]




    spectraImport <- mzR::peaks(mzML_con[[yeah]], scans = individualSpectrum)

    if(typeof(spectraImport) == "list"){

      spectraImport <- lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                                                        intensity = x[ , 2],
                                                                                        metaData = list(File = acquisitonInfo$rawFilePaths[[individualSpectrum]],
                                                                                                        Strain = ids[[yeah]])))
    } else if(typeof(spectraImport) == "double") {

      spectraImport <- MALDIquant::createMassSpectrum(mass = spectraImport[ , 1],
                                                      intensity = spectraImport[ , 2],
                                                      metaData = list(File = acquisitonInfo$rawFilePaths[[individualSpectrum]],
                                                                      Strain = ids[[yeah]]))
    }


    # Make sure to return spectraImport as a MassSpectrumList
    if(MALDIquant::isMassSpectrumList(spectraImport)){

    }else if(MALDIquant::isMassSpectrum(spectraImport)){
      spectraImport <- list(spectraImport)
    }




    if(max(MALDIquant::mass(spectraImport[[1]])) > 5000){ # if it's a protein spectrum


      spectraImport %>%
        serialize(object = ., 
                  connection = NULL,
                  ascii = FALSE,
                  xdr = FALSE,
                  version = 3) %>%
        memCompress(., type = "gzip") %>% 
        list(.)  -> sqlDataFrame$IndividualSpectra$proteinSpectrum

      
      
      # Why square root transformation and not log:
      #  Anal Bioanal Chem. 2011 Jul; 401(1): 167–181.
      # Published online 2011 Apr 12. doi:  10.1007/s00216-011-4929-z
      #"Especially for multivariate treatment of MALDI imaging data, the square root transformation can be considered for the data preparation
      #because for all three intensity groups in Table 1, the variance is approximately constant."

      spectraImport %>%
        MALDIquant::transformIntensity(., method = "sqrt") %>%
        MALDIquant::smoothIntensity(., method = "SavitzkyGolay", halfWindowSize = 20) %>%
        MALDIquant::removeBaseline(., method = "TopHat") %>%
        MALDIquant::detectPeaks(., method = "MAD", halfWindowSize = 20, SNR = 4) %>%
        serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
        memCompress(., type = "gzip") %>% 
        list(.)  -> sqlDataFrame$IndividualSpectra$proteinPeaks

      

    }else{
      ############
      #Spectra Preprocessing, Peak Picking
      spectraImport %>%
        serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
        memCompress(., type = "gzip") %>% 
        list(.) -> sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum


      spectraImport %>%
        MALDIquant::smoothIntensity(., method = "SavitzkyGolay", halfWindowSize = 20) %>%
        MALDIquant::removeBaseline(., method = "TopHat") %>%
        #Find all peaks with SNR >1, this will allow us to filter by SNR later, doesn't effect the peak-picking algorithm, just makes files bigger
        MALDIquant::detectPeaks(., method = "SuperSmoother", halfWindowSize = 20, SNR = 1) %>%
        serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
        memCompress(., type = "gzip") %>% 
        list(.) -> sqlDataFrame$IndividualSpectra$smallMoleculePeaks


      
      

      
    }

    
   sqlDataFrame$IndividualSpectra[1, ]
    
})



})

wqq <- unlist(wq, recursive = FALSE)
 
    conn <- pool::poolCheckout(userDBCon)
   
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







