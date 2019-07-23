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
  jsonlite::toJSON(input, digits = 5)
}


#' deserial
#'    Settings for serializing in IDBac (convert to json)
#' @param input matrix or vector
#'
#' @return JSON
#' @export

deserial <- function(input){
  jsonlite::fromJSON(input)
}


#' compress
#'   Settings for compressing raw vectors
#'
#' @param input raw vector to compressed
#' @param compression compression level 0-100
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




#' Take character, turn to raw, then compress 
#'
#' @param input character 
#' @param compression compression level 0-100
#'
#' @return NA
#' @export
#'
chartoRawtoCompressed <- function(input, compression){
  input <- base::enc2utf8(input)
  input <- base::charToRaw(input)
  IDBacApp::compress(input = input,
                     compression = compression)
}



# "From"getOS" code written by Will Lowe and copied from: http://conjugateprior.org/2015/06/identifying-the-os-from-r/

#' Get OS
#'
#' @param test for testing only
#'
#' @return text representing the user's os
#' @export
#'

getOS <- function(test = NULL){
  if (is.null(test)) {
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
  } else {
    return(test)
  }
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
#' @param path filepath of mzML or mzXML
#'
#' @return compressed, raw, character 
#' @export
#'
serializeXML <- function(path) {
  
  path <- readChar(path, nchars = file.info(path)$size, useBytes = T)
  IDBacApp::chartoRawtoCompressed(input = path,
                                  compression = 0)
  
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
  input <- gsub(" ", "_", input)
  input <- gsub("__", "_", input)
  return(input)
}







#' Create 384-well matrix map
#'
#' @return 384 well-like matrix, each element in matrix contains its position (eg col 1, row 3 contains "C4")
#' @export
#'
map384Well <- function(){
  aa <- sapply(1:24, function(x) paste0(LETTERS[1:16], x))
  matrix(aa, nrow = 16, ncol = 24,
         dimnames = list(LETTERS[1:16],
                         1:24)
  )
}


#' Create a 384-well matrix that is NA-filled
#'
#' @return 384-well matrix that is NA-filled
#' @export
#'
nulledMap384Well <- function() {
  a <- IDBacApp::map384Well()
  a[] <- NA
  as.data.frame(a)
}








#' Get default path for IDBac to save experiments to
#'
#' @return character path
#' @export 
#'
findIdbacHome <- function(){
  
  temp <- as.list(Sys.getenv())$HOME
  
  if (is.null(temp) || nchar(temp) < 2) {
    temp <- as.list(Sys.getenv())$LOCALAPPDATA
    if (is.null(temp) || nchar(temp) < 2 || length(temp) > 1) {
      temp <- getwd()
    }
  }
  
  temp <- file.path(temp, "IDBac_experiments")
  
  if(!dir.exists(temp)) {
    message(paste0("Creating directory 'IDBac_experiments' in ", dirname(temp)))
    dir.create(temp)
  }
  
  return(temp)
  
  
}