

# Given the path for a mzXML file ("singlemzXMLpath):
  # "findmzXMLsha" returns the mxML sha1 and fileSha1(s)
  # Assuming Bruker data, "findAcquisitionInfo" looks at the Acqu file and returns:
    # the Acqu file itself
    # the overall MassError
    # and the Acquisition date




findmzXMLsha1 <- function(singlemzXMLpath){

  sha <- new.env(parent = parent.frame())

  singlemzXMLpath %>%
    xml2::read_xml() %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_first(., "//mzXML/sha1" ) %>%
    xml2::xml_text() %>%
    return(.) -> sha$sha1

  sha
}

findmzXMLfilesha1 <- function(singlemzXMLpath){

  sha <- new.env(parent = parent.frame())

# This has to be done here in case "findAcquisitionInfo" isn't called
  singlemzXMLpath %>%
    xml2::read_xml() %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all(., "//mzXML/msRun/parentFile" ) %>%
    xml2::xml_attrs() %>%
    lapply(., function(x) as.character(x["fileSha1"])) %>%
    unlist() %>%
    return(.) -> sha$filesha1

  sha
}




findAcquisitionInfo <- function(singlemzXMLpath){


  sha <- new.env(parent = parent.frame())


  # Finds the "fileName" (file path() for the fid, the "filetype" and the "fileSha1"
  singlemzXMLpath %>%
    xml2::read_xml() %>%
    xml2::xml_ns_strip() %>%
    xml2::xml_find_all(., "//mzXML/msRun/parentFile" ) %>%
    xml2::xml_attrs() %>%
    return(.) -> p



  files <- unlist(lapply(p, function(x) as.character(x["fileName"])))
  files <- dirname(files)

  sha$rawFilePaths <- files

  sha$filesha1 <- unlist(lapply(p, function(x) as.character(x["fileSha1"])))


  #Bruker only::

  sha$Acqu <- lapply(files, function(x)  read.delim(file.path(x,"acqu"), sep="\n"))
  remove(files)


  sha$MassError <- unlist(lapply(sha$Acqu, function(x) as.character(x[grep("Masserr", x[,1]),]))) #Parse the Acqu file for the mass error row


  sha$MassError <- unlist(lapply(sha$MassError, function(x) as.numeric(strsplit(x, "##\\$Masserr= " )[[1]][[2]])))





  sha$AcquisitonDate <- unlist(lapply(sha$Acqu, function(x) as.character(x[grep("##\\$AQ_DATE", x[,1]),]))) #Parse the Acqu file for the mass error row


  sha$AcquisitonDate <- unlist(lapply(sha$AcquisitonDate, function(x) gsub('^.*<\\s*|\\s*.>.*$', '', x)))


  sha
}
