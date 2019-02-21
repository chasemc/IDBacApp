#' Collapse a sample's MALDIquant peak objects into a single peak object
#' 
#' @param peakList NA
#' @export
#' @return a single trimmed and binned MALDIquant peak object

smallMolDFtoNetwork <- function(peakList){
  
  smallNetwork <- MALDIquant::intensityMatrix(peakList)
  temp <- NULL
  
  for (i in 1:length(peakList)){
    temp <- c(temp,peakList[[i]]@metaData$Strain)
  }
  
  rownames(smallNetwork) <- temp
  smallNetwork[is.na(smallNetwork)] <- 0
  smallNetwork <- ifelse(smallNetwork > 0,1,0)
  smallNetwork <- as.data.frame(smallNetwork)
  #The network is weighted by the inverse of percentage of presence of the peak, this de-emphasizes commonly occuring peaks and "pulls" rarer peaks closer to their associated sample
  smallNetwork[ , colnames(smallNetwork)] <- sapply(smallNetwork[ , colnames(smallNetwork)],
                                                  function(x) ifelse(x == 1, 1 / sum(x), x))
  #Create a matrix readable by Gephi as an edges file
  smallNetwork <- cbind(rownames(smallNetwork),smallNetwork)
  smallNetwork <- reshape2::melt(smallNetwork)
  # Remove zero length edges
  smallNetwork <- base::subset(smallNetwork, smallNetwork$value != 0)
  colnames(smallNetwork) <- c("Source","Target","Weight")
  # Round m/z values to two decimals, use sprintf to preserve trailing zeros
  smallNetwork$Target <- sprintf(as.numeric(as.matrix(smallNetwork$Target)), fmt = '%#.2f')
  
  smallNetwork$Source <- as.character(smallNetwork$Source)
  smallNetwork$Target <- as.numeric(smallNetwork$Target)
  smallNetwork$Weight <- as.numeric(smallNetwork$Weight)
  
  return(smallNetwork)
  
}