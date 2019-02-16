#' colorBlindPalette
#'
#' @return colorblind palette and then rainbow x1000
#' @export

colorBlindPalette <- function(){
  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
    grDevices::rainbow(1000))
}



#' hashR
#'    Settings for hashing in IDBac
#' @param input object to hash 
#'
#' @return sha1 hash
#' @export

hashR <- function(input){
  digest::sha1(input, 
               digits = 14,
               zapsmall = 7,
               algo = "sha1",
               serialize = FALSE)
}


#' serial
#'    Settings for serializing in IDBac
#' @param input object to serialize
#'
#' @return serialized object in binary
#' @export

serial <- function(input){
  base::serialize(object = input ,
                  connection = NULL,
                  ascii = FALSE,
                  xdr = FALSE,
                  version = 3)
}


#' compress
#'   Settings for compressing in IDBac
#' @param input character or raw vector to compressed (char will be converted to raw)
#'
#' @return Raw vector
#' @export


compress <- function(input){
  base::memCompress(input, 
              type = "gzip")
}


#' decompress
#'    Settings for decompressing in IDBac, serialized called first
#' @param input compressed raw vector 
#'
#' @return raw vector 
#' @export

decompress <- function(input){
  base::memDecompress(input, 
              type = "gzip",
              asChar = FALSE)
}


#----
#' createSpectrumSha
#'    Given a mzR 
#'
#' @param  peaklist matrix
#'
#' @return sha
#' @export


createSpectrumSha <- function(peaklist){
  
  
  if(base::class(peaklist) != "matrix"){
    warning("createSpectrumSha: peakList given was not of type matrix")  
    
  } else {
    if(is.null(peaklist)){
      warning("No data found in mzML scan. If you think this is an error, please submit an issue to GitHub
          with an example file.")
    } else {
      
      return(IDBacApp::hashR(IDBacApp::serial(peaklist)))
    }
  }
}



# "From"getOS" code written by Will Lowe and copied from: http://conjugateprior.org/2015/06/identifying-the-os-from-r/

#' Get OS
#'
#' @return text representing the user's os
#' @export
#'

getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { 
    ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(as.character(tolower(os)))
}



#' Find mzML and mzXML files
#'
#' @param recursive search directories recursively? T/F
#' @param full full.names? T/F
#' @param inputPath path to search
#'
#' @return file paths of found files
#' @export
#'
findmz <- function(inputPath,
                   recursive = FALSE,
                   full = FALSE){
  # sets time limit outside though so dont use yet setTimeLimit(elapsed = 5, transient = FALSE)
  return(list.files(inputPath,
                    recursive = recursive,
                    full.names = full,
                    pattern = "\\.mz"))
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  
}



#' pipe
#'
#' @return NA
#' @export
#'
`%>%` <- function(){
  magrittr::`%>%`  
}
