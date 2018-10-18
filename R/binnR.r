
binnR <- function (vectorlist, ppm, low, high, increment){
  
  # Create a vector entry for every unique element
  longVector <- seq(from = low,
                    to = high,
                    by = increment)
  # length of vector that will be created
  nx <- length(longVector)
  # Adjust ppm to decimal tolarance across long vector
  toll <- ppm / 10e5 * longVector
  # Iterate over the provided list of vectors
  lapply(vectorlist, function(vec){
    
    matches <- rep(0, nx)
    ry <- 1:length(vec)
    for (i in seq_along(vec)) {
      # returns abs diff across entire long vector
      dif <- abs(longVector - vec[i])
      matches[which(dif <= toll)] <- 1
    }
    
    matches
    
  })
}