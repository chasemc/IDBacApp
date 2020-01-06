

#' Find a certain Bruker Acqus info
#'
#' @param acquisitonInformation parsed acqus
#' @param name acqus metadata name
#' @param type FUN.VALUE for vapply
#'
#' @return spot position vector
#' @export
#'
findBrukerInfo <- function(acquisitonInformation,
                           name,
                           type){
  
  vapply(acquisitonInformation,
         function(x) x[[name, exact = TRUE]],
         FUN.VALUE = type)
  
}

