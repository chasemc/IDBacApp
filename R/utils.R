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
createMZsha <- function(mzRobject){
  
  peaklist <- mzR::peaks(mzRobject)
  
  if(base::class(peaklist) =="double"){
    peaklist <- list(peaklist)
  }
  
  numScans <- length(peaklist)
  
  if(numScans==0){
    warning("No data found in mzML. If you think this is an error, please submit an issue to GitHub
          with an example file.")
  } else if (numScans == 1){
    peaklist <-  list(IDBacApp::hash(IDBacApp::serial(peaklist)))
  } else {
    peaklist <- lapply(peaklist, function(x) IDBacApp::hash(IDBacApp::serial(x)))
  }
  return(IDBacApp::hash(IDBacApp::serial(peaklist)))
}
