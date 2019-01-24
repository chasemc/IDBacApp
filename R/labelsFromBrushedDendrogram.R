#' networkViaBrushedDendrogram
#'
#' @param dendrogram dendrogram
#' @param brushYmin double, y-max of user-brush
#' @param brushYmax double, y-min of user-brush
#'
#' @return subset of labels that were brushed over in the dendrogram
#' @export
#'

labelsFromBrushedDendrogram <- function(dendrogram,
                                        brushYmin,
                                        brushYmax){
  
  # Takes "dendextend" dendrogram and user-brushed-input as input and returns sample_IDs corresponding to the brush
  
  #This takes a brush selection over the heirarchical clustering plot within the MAN tab and uses this selection of samples for MAN analysis
  location_of_Heirarchical_Leaves <- dendextend::get_nodes_xy(dendrogram)
  
  # See underneath for explanation of each column
  threeColTable <- data.frame(seq(1:length(labels(dendrogram))),
                              rep(1:length(labels(dendrogram))),
                              labels(dendrogram))
  #note: because rotated tree, x is actually y, y is actually x
  #column 1= y-values of dendrogram leaves
  #column 2= node x-values we selected (but only leaves, by only returning nodes with x-values of 0)
  #column 3= leaf labels
  
  # w = pull out the selected sample(s) indices based on the brush
  w <- which(threeColTable[ , 1] > brushYmin & threeColTable[ , 1] < brushYmax)
  # w = pull out the selected brushed sample(s)
  
  as.vector(threeColTable[ , 3][w])
  
}
