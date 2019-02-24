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
  digest::digest(input, 
                 algo = "xxhash64",
                 serialize = FALSE,
                 seed = 42)
}




#' serial
#'    Settings for serializing in IDBac
#' @param input object to serialize
#'
#' @return serialized object in binary
#' @export

serial <- function(input){
  RProtoBuf::serialize_pb(input, NULL)
}



#' compress
#'   Settings for compressing in IDBac
#' @param input character or raw vector to compressed (char will be converted to raw)
#'
#' @return Raw vector
#' @export


compress <- function(input){
  fst::compress_fst(input, 
                    compressor = "ZSTD",
                    compression = 50)
}


#' decompress
#'    Settings for decompressing in IDBac, serialized called first
#' @param input compressed raw vector 
#'
#' @return raw vector 
#' @export

decompress <- function(input){
  fst::decompress_fst(input)
}


#' Serialize a list of mzR peak matrices (one column at a time)
#'
#' @param mzRPeaks mzR::Peaks result
#' @param mass logical
#' @param intensity  logical 
#'
#' @return list of serials
#' @export
#'
mzRpeakSerializer <- function(mzRPeaks,
                              column = c("mass", "intensity")
                              ){
  
  switch(column,
         "mass" = {
           col <- 1
         },
         "intensity" = {
           col <- 2
         }
  )
  if (class(mzRPeaks) == "matrix") {
    if (ncol(mzRPeaks) == 2) {
      return(as.list(IDBacApp::compress(IDBacApp::serial(mzRPeaks[, col]))))
    }
  } else if (class(mzRPeaks) == "list") {
    return(
      lapply(mzRPeaks, 
             function(x){
               IDBacApp::compress(IDBacApp::serial(x[, col]))
             }
      )
    )
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




