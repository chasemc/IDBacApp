#' colorBlindPalette
#'
#' @return colorblind palette and then rainbow x1000
#' 

colorBlindPalette <- function(){
  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
    grDevices::rainbow(1000))
}



#' hashR
#'    Settings for hashing in IDBac
#' @param input object to hash 
#'
#' @return sha1 hash
#' 

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
#' 

serial <- function(input){
  jsonlite::toJSON(input, digits = 5)
}


#' deserial spectrum json (single json array)
#'    Settings for serializing in IDBac (convert to json)
#' @param input matrix or vector
#'
#' @return dataframe
#' 

deserial <- function(input){
  scan(text =  gsub("\\[|\\]", "", input),
       sep =",", 
       what = double(),
       quiet = T)
}


#' compress
#'   Settings for compressing raw vectors
#'
#' @param input raw vector to compressed
#' @param compression compression level 0-100
#'
#' @return Raw vector
#' 
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
#' 

decompress <- function(input){
  fst::decompress_fst(input)
}




#' Take character, turn to raw, then compress 
#'
#' @param input character 
#' @param compression compression level 0-100
#'
#' @return NA
#' 
#'
chartoRawtoCompressed <- function(input, compression){
  input <- base::enc2utf8(input)
  input <- base::charToRaw(input)
  compress(input = input,
           compression = compression)
}



# "From"getOS" code written by Will Lowe and copied from: http://conjugateprior.org/2015/06/identifying-the-os-from-r/

#' Get OS
#'
#' @param test for testing only
#'
#' @return text representing the user's os
#' 
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
#' @param path path to search
#'
#' @return file paths of found files
#' 
#'
find_mz_files <- function(path,
                          recursive = FALSE,
                          full = FALSE){
  # sets time limit outside though so dont use yet setTimeLimit(elapsed = 5, transient = FALSE)
  return(list.files(path,
                    recursive = recursive,
                    full.names = full,
                    pattern = "\\.mz"))
  #setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  
}



#' Read mzXML, xml and transform to raw character for storing in SQLite
#'
#' @param path filepath of mzML or mzXML
#'
#' @return compressed, raw, character 
#' 
#'
serializeXML <- function(path) {
  
  path <- readChar(path, nchars = file.info(path)$size, useBytes = T)
  chartoRawtoCompressed(input = path,
                        compression = 0)
  
}



#' Create 384-well matrix map
#'
#' @return 384 well-like matrix, each element in matrix contains its position (eg col 1, row 3 contains "C4")
#' 
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
#' 
#'
nulledMap384Well <- function() {
  a <- map384Well()
  a[] <- NA
  as.data.frame(a)
}




#' Checkout pool if it isn't
#'
#' @param con db pool/connection
#'
#' @return checked out pool
#' 
#'
poolToCon <- function(con) {
  
  type <- class(con)
  
  if ("Pool" %in% type) {
    
    return(pool::poolCheckout(con))
    
  } else  if ("SQLiteConnection" %in% type) {
    
    return(con)
    
  } else {
    
    stop("Expected either a pool or SQLite object.")
    
  }
  
  
}




#' Check if a pool object
#'
#' @param pool variable to check
#' @return NA, stops function if not a pool object
.checkPool <- function(pool){
  
  val <- all(inherits(pool, "Pool"),
             inherits(pool, "R6"))
  
  if (isFALSE(val)) {
    # paste0(deparse(sys.calls()[[sys.nframe()-1]]) gets the info of the calling function
    stop(paste0(deparse(sys.calls()[[sys.nframe() - 1]]),
                " expected a pool object"))
  }
  
}



#' Find database path from pool object
#'
#' @param pool {pool} object
#'
#' @return Path to pool's database
.db_path_from_pool <- function(pool){
  
  .checkPool(pool = pool)
  
  provided_db_path <- normalizePath(pool$fetch()@dbname,
                                    winslash = "/")
  
  if (file.exists(provided_db_path)) {
    return(provided_db_path)
  } else {
    stop("\n",
         "Couldn't find:",
         "\n",
         provided_db_path)
  }
}


