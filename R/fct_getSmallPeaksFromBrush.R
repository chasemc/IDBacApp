#' Retrieve small molecule and matrix peak lists and make consensus peak lists
#'
#' @param pool pool
#' @param sampleIDs character vector of IDs (currently not used)
#' @param dendrogram protein dendrogram that was brushed
#' @param matrixIDs sampleID that will be used as a matrix
#' @param minFrequency peaks whose frequency of occurence in replicates below this are dropped
#' @param lowerMassCutoff lower mass to retain
#' @param upperMassCutoff upper mass to retain
#' @param minSNR minimum SNR a peak must have to be retained
#' @param brushInputs brusked dendrogram outputs
#'
#' @return a list containg two lists of MALDIquant peak objects "samplePeaks" and "matrixPeaks"
.getSmallPeaksFromBrush <- function(pool,
                                    sampleIDs = NULL,
                                    dendrogram,
                                    brushInputs,
                                    matrixIDs = NULL,
                                    minFrequency,
                                    lowerMassCutoff,
                                    upperMassCutoff,
                                    minSNR) {
  # TODO split the brush out into own function
  if (is.null(sampleIDs)) {
    if (!is.null(dendrogram)) {
      # If there is a protein dendrogram but a user hasn't brushed:
      if (is.null(brushInputs()$ymin)) {
        # Don't ovrwhelm the browser by displaying everthing when page loads
        if (length(labels(dendrogram)) >= 25) {
          # If more than 25 strains present, only display 10 to start, otherwise display all
          # Get random 10 strain IDs from dendrogram
          sampleIDs <- labels(dendrogram)[1:sample.int(10, 1)]
        } else {
          sampleIDs <- labels(dendrogram)
        }
      } else {
        # Get the labels of the brushed dendrogram
        sampleIDs <- labelsFromBrushedDendrogram(
          dendrogram = dendrogram,
          brushYmin = brushInputs()$ymin,
          brushYmax = brushInputs()$ymax
        )
      }
    } else {
      # All mall moleculesamples
      sampleIDs <- NULL
    }
  }
  print("hit")
  print(sampleIDs)
  print("hit2")
  if (length(sampleIDs) == 0) {
    sampleIDs <- NULL
  }
  idbac_get_peaks(
    pool = pool,
    sampleIDs = sampleIDs,
    minFrequency = minFrequency,
    lowerMassCutoff = lowerMassCutoff,
    upperMassCutoff = upperMassCutoff,
    minSNR = minSNR,
    tolerance = 0.002,
    type = "small",
    mergeReplicates = TRUE
  )
}
