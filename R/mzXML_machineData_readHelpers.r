

# Given the path for a mzML file ("singlemzMLpath):
  # "findmzMLsha" returns the mxML sha1 and fileSha1(s)
  # Assuming Bruker data, "findAcquisitionInfo" looks at the Acqu file and returns:
    # the Acqu file itself
    # the overall MassError
    # and the Acquisition date




findmzMLsha1 <- function(singlemzMLpath){

  sha <- new.env(parent = parent.frame())

  singlemzMLpath %>%
    xml2::read_xml() %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_first(., "//mzML/sha1" ) %>%
    xml2::xml_text() %>%
    return(.) -> sha$sha1

  sha
}





findAcquisitionInfo <- function(singlemzMLpath,
                                manufacturer){


  sha <- new.env(parent = parent.frame())


  # Finds the "fileName" (file path() for the fid, the "filetype" and the "fileSha1"
  singlemzMLpath %>%
    xml2::read_xml() %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all(., "//mzML/msRun/parentFile" ) %>%
    xml2::xml_attrs() %>%
    return(.) -> p



  files <- unlist(lapply(p, function(x) as.character(x["fileName"])))
  files <-  gsub("file://", "", files)
  files <- dirname(files)
  sha$rawFilePaths <- files
  sha$filesha1 <- unlist(lapply(p, function(x) as.character(x["fileSha1"])))


  #Bruker only::

  tryCatch({

  if(manufacturer == "Bruker Daltonics"){
    files <- files[which(file.exists(files))]
    if(length(files) > 0){
      sha$Instrument_MetaFile  <- lapply(files, function(x)  read.delim(file.path(x,"acqu"), sep="\n")) # Find Acqu file
      sha$MassError <- unlist(lapply(sha$Instrument_MetaFile , function(x) as.character(x[grep("Masserr", x[,1]),]))) #Parse the Acqu file for the mass error row
      sha$MassError <- unlist(lapply(sha$MassError, function(x) as.numeric(strsplit(x, "##\\$Masserr= " )[[1]][[2]])))
      sha$AcquisitionDate <- unlist(lapply(sha$Instrument_MetaFile , function(x) as.character(x[grep("##\\$AQ_DATE", x[,1]),]))) #Parse the Acqu file for the mass error row
      sha$AcquisitionDate <- unlist(lapply(sha$AcquisitionDate, function(x) gsub('^.*<\\s*|\\s*.>.*$', '', x)))
    }
  }
    }, error = (function(e) warning(e)))


  sha
}
