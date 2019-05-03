
#' Find which MALDI-target spots have data but an ID wasn't assigned 
#'
#' @param spots spot locations that have data 
#' @param sampleMap 
#'
#' @return
#' @export
#'
#' @examples
findMissingSampleMapIds <- function(spots, 
                                    sampleMap){
  
  if (is.character(spots)) {
    
    if (is.data.frame(sampleMap)) {
      # create sample map
      aa <- IDBacApp::map384Well()
      s1 <- base::as.matrix(sampleMap)
      # Which sample locations have data but weren't assigned an ID?
      b <- sapply(spots, function(x) s1[which(aa %in% x)])
      # Return as character vector of spot locations
      as.character(spots[which(is.na(b))])
      
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
}