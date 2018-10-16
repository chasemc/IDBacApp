

# Given the path for a mzML file ("singlemzMLpath):
  # "findmzMLsha" returns the mxML sha1 and spectrumSHA(s)
  # Assuming Bruker data, "findAcquisitionInfo" looks at the Acqu file and returns:
    # the Acqu file itself
    # the overall MassError
    # and the Acquisition date




findRawSHAandFile <- function(singlemzMLpath){
  if(!exists("sha")){
    sha <- new.env(parent = parent.frame())
  }
  
  pp <- xml2::read_xml(singlemzMLpath)
  pp <- xml2::xml_ns_strip(pp)
  pp2 <- xml2::xml_find_all(pp,"//sourceFileList/sourceFile")  
  pp1 <- xml2::xml_contents(pp2)
  sha$spectrumSHA <- grep("MS:1000569",pp1) # tag for sha1
  sha$spectrumSHA <- xml2::xml_attrs(pp1)[sha$spectrumSHA]  # get sha1 nodes
  sha$spectrumSHA <- unlist(lapply(sha$spectrumSHA, function(x) as.list(x)$value)) # get sha1 values
  sha$rawFilePaths <- unlist(lapply(pp2, function(x) as.list(xml2::xml_attrs(x))$location)) # get raw filepath location

  pp <- xml2::read_xml(singlemzMLpath)
  pp <- xml2::xml_ns_strip(pp)
  pp <- xml2::xml_find_all(pp,"//referenceableParamGroup/cvParam") 

  sha$manufacturer <- as.list(xml2::xml_attrs(pp)[[1]])$name
  
  
  sha
}












  findAcquisitionInfo <- function(rawFilepaths,
                                manufacturer){

    if(!exists("sha")){
      sha <- new.env(parent = parent.frame())
    }
  
  files <-  gsub("file://", "", rawFilepaths)
  files <- dirname(files)

  #gat Bruker flex series metadata:

  tryCatch({

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
  }
    }, error = (function(e) warning(e)))


  sha
}
