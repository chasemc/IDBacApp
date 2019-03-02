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
#'    Settings for serializing in IDBac (convert to json)
#' @param input matrix or vector
#'
#' @return JSON
#' @export

serial <- function(input){
  jsonlite::toJSON(input)
}



#' compress
#'   Settings for compressing raw vectors
#' @param input raw vector to compressed
#'
#' @return Raw vector
#' @export


compress <- function(input, compression = 0){
  fst::compress_fst(input, 
                    compressor = "ZSTD",
                    compression = compression)
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




#' Take character, turn to raw, then compress (note: base::charToRaw is not vectorized)
#'
#' @param input character 
#' @param compression 
#'
#' @return
#' @export
#'
#' @examples
chartoRawtoCompressed <- function(input, compression){
  input <- base::charToRaw(input)
  IDBacApp::compress(input = input,
                     compression = compression)
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



#' Read mzXML, XML and transform to raw character for storing in SQLite
#'
#' @param path xml2 connection
#'
#' @return raw 
#' @export
#'
serializeXML <- function(path) {
  path <- IDBacApp::readXML(path)
  path <- base::as.character(path)
  IDBacApp::chartoRawtoCompressed(input = path,
                                  compression = 100)
  
}


#' ead mzXML, XML as XML
#'
#' @param path file path to mzML or mzXML
#'
#' @return xml2 connection
#' @export
#'
readXML <- function(path) {
  xml2::read_xml(path)
}




#' People have trouble with spaces
#'
#' @param input character vector 
#'
#' @return character vector
#' @export
#'
cleanWSpace <- function(input){
  
  input <- trimws(input)
  return(gsub(" ", "_", input))
}

