# -----------------
spectraProcessingFunction <- function(rawDataFilePath, userDBCon){
library(magrittr)

  # Open connection to single mzML file, but don't read (memory pointer)
  mzML_con <- mzR::openMSfile(file = rawDataFilePath)
  # Generate base SQL table
  sqlDataFrame <- IDBacApp::sqlTableArchitecture()

  # Find sample name (fileName)
  sampleName <- tools::file_path_sans_ext(basename(rawDataFilePath))
 
  
  # Make "sampleName" the "Strain_ID".
  sqlDataFrame$metaData$Strain_ID <- sampleName


  
   conn <- pool::poolCheckout(userDBCon)
  # Write to SQL DB  (There is no sample level metadata to add at this point)
  DBI::dbWriteTable(conn = conn,
                    name = "metaData", # SQLite table to insert into
                    sqlDataFrame$metaData[1, ], # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite

  pool::poolReturn(conn)
  

  

  #----------------------------------------------
  #----------------------------------------------
  # Create SQL "XML" table entry

  # Get instrument Info
  instInfo <- mzR::instrumentInfo(mzML_con)

  sqlDataFrame$XML$manufacturer  <- instInfo$manufacturer
  sqlDataFrame$XML$model         <- instInfo$model
  sqlDataFrame$XML$ionisation    <- instInfo$ionisation
  sqlDataFrame$XML$analyzer      <- instInfo$analyzer
  sqlDataFrame$XML$detector      <- instInfo$detector

  remove(instInfo)


  

  mzMLSHA <- xml2::read_xml(rawDataFilePath)
  mzMLSHA <- xml2::xml_serialize(mzMLSHA, NULL)
  mzMLSHA <- digest::sha1(mzMLSHA)
  
  

  sqlDataFrame$XML$mzMLSHA <- mzMLSHA



  # Get mzML, serialize, compress, for insert to SQL


  rawDataFilePath %>%
    readLines %>%
    serialize(., NULL) %>%
    memCompress(., type = "gzip") %>%
    list(.) %>%
    return(.) -> sqlDataFrame$XML$XML

  # Find individual spectra SHA and raw data filepath from mzML
  individualRawSpecSHA <- IDBacApp::findRawSHAandFile(rawDataFilePath)
  
  
  # Find acquisitonInfo from mzML file
  acquisitonInfo <- IDBacApp::findAcquisitionInfo(individualRawSpecSHA$rawDataFilePath,
                                                  individualRawSpecSHA$manufacturer)

  # Get acqus file as list of blobs
  acquisitonInfo$Instrument_MetaFile %>%
    serialize(object = .,
              connection = NULL,
              ascii = FALSE,
              xdr = FALSE,
              version = 3) %>%
    memCompress(.,
                type="gzip") %>%
    list(.) %>%
    return(.) -> sqlDataFrame$XML$Instrument_MetaFile

  conn <- pool::poolCheckout(userDBCon)
  
  # Write to SQL DB
  DBI::dbWriteTable(conn = conn,
                    name = "XML", # SQLite table to insert into
                    sqlDataFrame$XML[1, ], # Insert single row into DB
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

indspec <- lapply(1:nrow(mzR::header(mzML_con)), function(individualSpectrum){
#  for(individualSpectrum in 1:nrow(mzR::header(mzML_con))){

    # Reset
    sqlDataFrame <- IDBacApp::sqlTableArchitecture()


    sqlDataFrame$IndividualSpectra$mzMLSHA <- mzMLSHA
    sqlDataFrame$IndividualSpectra$Strain_ID <- sampleName
    sqlDataFrame$IndividualSpectra$spectrumSHA <- individualRawSpecSHA$spectrumSHA[[individualSpectrum]]
    sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[individualSpectrum]]
    sqlDataFrame$IndividualSpectra$AcquisitionDate <- acquisitonInfo$AcquisitionDate[[individualSpectrum]]






    spectraImport <- mzR::peaks(mzML_con, scans = individualSpectrum)

    if(typeof(spectraImport) == "list"){

      spectraImport <- lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                                                        intensity = x[ , 2],
                                                                                        metaData = list(File = acquisitonInfo$rawFilePaths[[individualSpectrum]],
                                                                                                        Strain = sampleName)))
    } else if(typeof(spectraImport) == "double") {

      spectraImport <- MALDIquant::createMassSpectrum(mass = spectraImport[ , 1],
                                                      intensity = spectraImport[ , 2],
                                                      metaData = list(File = acquisitonInfo$rawFilePaths[[individualSpectrum]],
                                                                      Strain = sampleName))
    }





    # Make sure to return spectraImport as a MassSpectrumList
    if(MALDIquant::isMassSpectrumList(spectraImport)){

    }else if(MALDIquant::isMassSpectrum(spectraImport)){
      spectraImport <- list(spectraImport)
    }





    if(max(MALDIquant::mass(spectraImport[[1]])) > 10000){ # if it's a protein spectrum




      spectraImport %>%
        serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
        memCompress(., type="gzip") %>%
        list(.) %>%
        return(.) -> sqlDataFrame$IndividualSpectra$proteinSpectrum

      
      
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
        memCompress(., type="gzip") %>%
        list(.) %>%
        return(.) -> sqlDataFrame$IndividualSpectra$proteinPeaks

    


    }else{
      ############
      #Spectra Preprocessing, Peak Picking
      spectraImport %>%
        serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
        memCompress(., type="gzip") %>%
        list(.) %>%
        return(.) -> sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum


      spectraImport %>%
        MALDIquant::smoothIntensity(., method = "SavitzkyGolay", halfWindowSize = 20) %>%
        MALDIquant::removeBaseline(., method = "TopHat") %>%
        #Find all peaks with SNR >1, this will allow us to filter by SNR later, doesn't effect the peak-picking algorithm, just makes files bigger
        MALDIquant::detectPeaks(., method = "SuperSmoother", halfWindowSize = 20, SNR = 1) %>%
        serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
        memCompress(., type="gzip") %>%
        list(.) %>%
        return(.) -> sqlDataFrame$IndividualSpectra$smallMoleculePeaks

  

      
    }

    
    sqlDataFrame$IndividualSpectra[1, ]
    
})


  

   conn <- pool::poolCheckout(userDBCon)
 
    for(i in seq_along(indspec)){
       
    # Write to SQL DB
    DBI::dbWriteTable(conn = conn,
                      name = "IndividualSpectra", # SQLite table to insert into
                      indspec[[i]], # Insert single row into DB
                      append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite
}
    pool::poolReturn(conn)
    













}
