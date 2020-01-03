#' Get default path for IDBac to save experiments to
#'
#' @return character path
#' @export 
#'
findIdbacHome <- function(){
  if (isTRUE(.Options$idbac_testing)) {
    temp <- file.path(tempdir(),
                      "testing")
    suppressWarnings(dir.create(temp))
    
    temp <- file.path(temp,
                      "idbac_experiments")
    
    suppressWarnings(dir.create(temp))
    
    temp <- normalizePath(temp,
                          winslash = "/",) 
    message(paste0("IDBac testing mode... \n \t Creating sqlDirectory in: \n \t \t ",
                   temp)
    )
    return(temp)
  } else {
    temp <- as.list(Sys.getenv())$HOME
    temp <- file.path(temp,
                      "Documents")
    if (!dir.exists(temp)) {
      temp <- as.list(Sys.getenv())$LOCALAPPDATA
      if (!dir.exists(temp) || length(temp) > 1) {
        temp <- getwd()
      }
    }
    
    temp <- file.path(temp, "idbac_experiments", fsep = "/")
    
    if (!dir.exists(temp)) {
      message(paste0("Creating directory 'idbac_experiments' in ", dirname(temp)))
      dir.create(temp)
    }
    
    return(normalizePath(temp, winslash = "/"))
    
  }
}