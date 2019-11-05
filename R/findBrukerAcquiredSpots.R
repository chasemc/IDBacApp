

#' Find a certain Bruker Acqus info
#'
#' @param acquisitonInformation parsed acqus
#' @param name acqus metadata name
#'
#' @return spot position vector
#' @export
#'
findBrukerInfo <- function(acquisitonInformation,
                           name){
  
  unlist(lapply(acquisitonInformation,
                function(x){
                  x[[name]]
                })
  )
}