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
#' createMZsha
#'    Given a mzR 
#' @param  peaklist matrix
#'
#' @return sha
#' @export


createMZsha <- function(peaklist){

  if(base::class(peaklist) == "double"){
    peaklist <- list(peaklist)
  }
  
  if(base::class(peaklist) == "matrix"){
    peaklist <- list(peaklist)
  }
  
  numScans <- length(peaklist)
  
  if(numScans==0){
    warning("No data found in mzML. If you think this is an error, please submit an issue to GitHub
          with an example file.")
  } else if (numScans == 1){
    peaklist <-  list(IDBacApp::hashR(IDBacApp::serial(peaklist)))
  } else {
    peaklist <- lapply(peaklist, function(x) IDBacApp::hashR(IDBacApp::serial(x)))
  }
  return(IDBacApp::hashR(IDBacApp::serial(peaklist)))
}





# "From"getOS" code written by Will Lowe and copied from: http://conjugateprior.org/2015/06/identifying-the-os-from-r/

getOS <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(as.character(tolower(os)))
  }
