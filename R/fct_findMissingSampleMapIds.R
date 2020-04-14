#' Find which MALDI-target spots have data but an ID wasn't assigned 
#'
#' @param spots spot locations that have data 
#' @param sampleMap user-input sample names mapped to 384-well plate (16 rows, 24 columns)
#' @param ignoreMissing TRUE/FALSE whether to ignore target spots missing ids
#'
#' @return character vector of missing sample IDs or character(0)
#' 
#'
findMissingSampleMapIds <- function(spots, 
                                    sampleMap,
                                    ignoreMissing){
  
  if (dims(sampleMap) != c(16, 24)) {
    stop("Please provide a 16-row by 24-column matrix")
  }
  
  if (is.character(spots)) {
    
    
    # sampleMap has to be df for display in shiny, but will need to be
    # converted to matrix for the functions below
    if (is.data.frame(sampleMap) || is.matrix(sampleMap)) {
      # create sample map
      plateMap <- map384Well()
      # Which sample locations have data but weren't assigned an ID?
      sampleMap <- base::as.matrix(sampleMap)
      b <- sampleMap[match(spots, plateMap)]
      
    } else {
      warning("'findMissingSampleMapIds(spots = )' expected data.frame input \n \n",
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
  
  missing <- unique(b[which(is.na(b))])
  
  if (isTRUE(ignoreMissing)) {
    return(list(missing = missing,
                matching = b))
  } else {
    
    b[which(is.na(b))] <- paste0("Spot-", b[which(is.na(b))])
    return(list(missing = missing,
                matching = b))
  }
  
  
}