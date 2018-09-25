# -----------------
spectraProcessingFunction <- function(rawDataFilePaths,idbacDirectory, userDBCon){

  # "rawDataFilePaths" is one mzXML path
  # "idbacDirectory"  is the path of the IDBac data directory

  # Open connection to mzXML but don't read
  mzxml_con <- mzR::openMSfile(file = rawDataFilePaths)

  # Find sample name (fileName)
  sampleName <- tools::file_path_sans_ext(basename(rawDataFilePaths))




  # Generate base SQL table
  sqlDataFrame <- IDBacApp::sqlTableArchitecture()



  #----------------------------------------------
  #----------------------------------------------
  # Create SQL "XML" table entry

  # Get instrument Info
  instInfo <- mzR::instrumentInfo(mzxml_con)

  sqlDataFrame$XML$manufacturer  <- instInfo$manufacturer
  sqlDataFrame$XML$model         <- instInfo$model
  sqlDataFrame$XML$ionisation    <- instInfo$ionisation
  sqlDataFrame$XML$analyzer      <- instInfo$analyzer
  sqlDataFrame$XML$detector      <- instInfo$detector

  remove(instInfo)


  # Get mzxml sha and filesha1
  sha1 <- IDBacApp::findmzXMLsha1(rawDataFilePaths)$sha1


  sqlDataFrame$XML$SHA1 <- sha1



  # Get mzxml, serialize, compress, for insert to SQL


  rawDataFilePaths %>%
    readLines %>%
    serialize(., NULL) %>%
    memCompress(., type = "gzip") %>%
    list(.) %>%
    return(.) -> sqlDataFrame$XML$XML


  # Write to SQL DB
  DBI::dbWriteTable(conn = userDBCon,
                    name = "XML", # SQLite table to insert into
                    sqlDataFrame$XML[1, ], # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite





  #----------------------------------------------
  #----------------------------------------------
  # Create SQL "IndividualSpectra" entry

  #----------------------------------------------
  # Assign information about the acquisition





  acquisitonInfo <- IDBacApp::findAcquisitionInfo(rawDataFilePaths,
                                                    sqlDataFrame$XML$manufacturer)

  sqlDataFrame$XML <-NULL # Free up memory


  check <- length(acquisitonInfo$Acqu) == length(acquisitonInfo$AcquisitionDate) &&
    length(acquisitonInfo$Acqu) == length(acquisitonInfo$filesha1) &&
    length(acquisitonInfo$Acqu) == length(acquisitonInfo$MassError)



if(check == FALSE){
  warning("spectraProcessingFunction.r: Acquisiton information elements are of different length")
}



#------------------------------


for(oneReplicate in 1:length(acquisitonInfo$filesha1)){

  # Reset
  sqlDataFrame <- IDBacApp::sqlTableArchitecture()


  sqlDataFrame$IndividualSpectra$filesha1 <- acquisitonInfo$filesha1[[oneReplicate]]
  sqlDataFrame$IndividualSpectra$SHA1 <- sha1 # only one
  sqlDataFrame$IndividualSpectra$Strain_ID <- sampleName # only one
  sqlDataFrame$IndividualSpectra$MassError <- acquisitonInfo$MassError[[oneReplicate]]
  sqlDataFrame$IndividualSpectra$AcquisitionDate <- acquisitonInfo$AcquisitionDate[[oneReplicate]]


  acquisitonInfo$Acqu[[oneReplicate]] %>%
    serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
    memCompress(., type="gzip") %>%
    list(.) %>%
  return(.) -> sqlDataFrame$IndividualSpectra$Acqu





  spectraImport <- mzR::peaks(mzxml_con, scans = oneReplicate)

  if(typeof(spectraImport) == "list"){

    spectraImport <- lapply(spectraImport, function(x) MALDIquant::createMassSpectrum(mass = x[ , 1],
                                                                                      intensity = x[ , 2],
                                                                                      metaData = list(File = acquisitonInfo$rawFilePaths[[oneReplicate]],
                                                                                                      Strain = sampleName)))
  } else if(typeof(spectraImport) == "double") {

    spectraImport <- MALDIquant::createMassSpectrum(mass = spectraImport[ , 1],
                                                    intensity = spectraImport[ , 2],
                                                    metaData = list(File = acquisitonInfo$rawFilePaths[[oneReplicate]],
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


  sqlDataFrame$IndividualSpectra$proteinSpectrumHash <- digest::digest(sqlDataFrame$IndividualSpectra$proteinSpectrum, algo= "sha256")

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


  sqlDataFrame$IndividualSpectra$proteinPeaksHash <- digest::digest(sqlDataFrame$IndividualSpectra$proteinPeaks, algo= "sha256")



  }else{
    ############
    #Spectra Preprocessing, Peak Picking
    spectraImport %>%
      serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
      memCompress(., type="gzip") %>%
      list(.) %>%
    return(.) -> sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum

    sqlDataFrame$IndividualSpectra$smallMoleculeSpectrumHash <- digest::digest(sqlDataFrame$IndividualSpectra$smallMoleculeSpectrum, algo= "sha256")

    spectraImport %>%
      MALDIquant::smoothIntensity(., method = "SavitzkyGolay", halfWindowSize = 20) %>%
      MALDIquant::removeBaseline(., method = "TopHat") %>%
      #Find all peaks with SNR >1, this will allow us to filter by SNR later, doesn't effect the peak-picking algorithm, just makes files bigger
    MALDIquant::detectPeaks(., method = "SuperSmoother", halfWindowSize = 20, SNR = 1) %>%
      serialize(object = ., connection = NULL, ascii = FALSE, xdr = FALSE, version = 3) %>%
      memCompress(., type="gzip") %>%
      list(.) %>%
    return(.) -> sqlDataFrame$IndividualSpectra$smallMoleculePeaks


    sqlDataFrame$IndividualSpectra$smallMoleculePeaksHash <- digest::digest(sqlDataFrame$IndividualSpectra$smallMoleculePeaks, algo= "sha256")

  }

  remove(spectraImport)


  # Write to SQL DB
  DBI::dbWriteTable(conn = userDBCon,
                    name = "IndividualSpectra", # SQLite table to insert into
                    sqlDataFrame$IndividualSpectra[1, ], # Insert single row into DB
                    append = TRUE, # Append to existing table
                    overwrite = FALSE) # Do not overwrite




}










}
