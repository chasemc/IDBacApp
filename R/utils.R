#----
colorBlindPalette <- function(){
  c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
    rainbow(1000))
}



#----
hashR <- function(input){
  digest::sha1(input, 
               digits = 14,
               zapsmall = 7,
               algo = "sha1",
               serialize = FALSE)
}

#----
serial <- function(input){
  base::serialize(object = input ,
                  connection = NULL,
                  ascii = FALSE,
                  xdr = FALSE,
                  version = 3)
}

#----
compress <- function(input){
  memCompress(input, 
              type = "gzip")
}

#----
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
