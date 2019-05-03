#' Find which MALDI-target spots have data but an ID wasn't assigned 
#'
#' @param spots spot locations that have data 
#' @param sampleMap user-input sample names mapped to 384-well plate
#'
#' @return character vector of missing sample IDs or character(0)
#' @export
#'
findMissingSampleMapIds <- function(spots, 
                                    sampleMap){
  
  if (is.character(spots)) {
    
    # sampleMap has to be df for display in shiny, but will need to be
    # converted to matrix for the functions below
    if (is.data.frame(sampleMap)) {
      # create sample map
      plateMap <- IDBacApp::map384Well()
      # Which sample locations have data but weren't assigned an ID?
      sampleMap <- base::as.matrix(sampleMap)
      b <- sapply(spots, function(x) sampleMap[which(plateMap %in% x)])
      
    } else {
      warning("'findMissingSampleMapIds(sampleMap = )' expected data.frame input \n \n",
              "'provided input:' \n",
              sampleMap, 
              "\n \n")
    }
  } else {
    warning("'findMissingSampleMapIds(sampleMap = )' expected character input \n \n",
            "'provided input:' \n",
            spots, 
            "\n \n")
  }
  names(which(is.na(b)))
}