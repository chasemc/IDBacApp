# IDBacApp::subtractMatrixBlank(sampleIds = labels(selectedSmallMolPeakList()), 
#                               peakList = selectedSmallMolPeakList(),
#                               binTolerance = 0.002)










#' Retrieve small molecule and matrix peak lists and make consensus peak lists
#'
#' @param pool pool
#' @param sampleIDs character vector of IDs (currently not used)
#' @param dendrogram protein dendrogram that was brushed 
#' @param ymin brush y-min
#' @param ymax brush y-max
#' @param matrixIDs sampleID that will be used as a matrix
#' @param peakPercentPresence peaks whose frequency of occurence in replicates below this are dropped
#' @param lowerMassCutoff lower mass to retain
#' @param upperMassCutoff upper mass to retain
#' @param minSNR minimum SNR a peak must have to be retained
#'
#' @return a list containg two lists of MALDIquant peak objects "samplePeaks" and "matrixPeaks"
#' @export
#'
getSmallMolSpectra <- function(pool,
                               sampleIDs,
                               dendrogram,
                               ymin,
                               ymax,
                               matrixIDs = NULL,
                               peakPercentPresence,
                               lowerMassCutoff,
                               upperMassCutoff, 
                               minSNR){
  
  checkedPool <- pool::poolCheckout(pool)
  

  
  # Check if there is a protein dendrogram, if TRUE: use to subset strains, if FALSE, show all MAN
  if(!is.null(dendrogram)){  
    
    # If there is a protein dendrogram but a user hasn't brushed:
    if(is.null(ymin)){ 
      
      # Don't ovrwhelm the browser by displaying everthing when page loads
      if(length(labels(dendrogram)) >= 25){
        # If more than 25 strains present, only display 10 to start, otherwise display all
        # Get random 10 strain IDs from dendrogram
        sampleIDs <- labels(dendrogram)[1:sample.int(10, 1)]
      } else {
        sampleIDs <- labels(dendrogram)
      }
    } else {
      # Get the labels of the brushed dendrogram
      sampleIDs <- IDBacApp::labelsFromBrushedDendrogram(dendrogram = dendrogram,
                                                     #    dendrogramShortLabels = dendrogram,
                                                         brushYmin = ymin,
                                                         brushYmax = ymax)
    }
  } else {
    
    # retrieve all Strain_IDs in db that have small molecule spectra
    sampleIDs <- glue::glue_sql("SELECT DISTINCT `Strain_ID`
                               FROM `IndividualSpectra`
                               WHERE (`smallMoleculePeaks` IS NOT NULL)",
                                .con = checkedPool
    )
    
    sampleIDs <- DBI::dbGetQuery(checkedPool, sampleIDs)
    sampleIDs <- as.vector(sampleIDs)
  }
  
  
  s2 <<- checkedPool
  s1<<-sampleIDs
  
  samples <- lapply(sampleIDs, 
                    function(sampleIDs){ 
                      IDBacApp::collapseSmallMolReplicates(checkedPool = checkedPool,
                                                           sampleIDs = sampleIDs ,
                                                           peakPercentPresence = peakPercentPresence,
                                                           lowerMassCutoff =lowerMassCutoff,
                                                           upperMassCutoff = upperMassCutoff, 
                                                           minSNR = minSNR)
                    })
  
  
  
  
  
  if (!is.null(matrixIDs)){
    
    # Not lapply because we are just going to merge anyways
    
    matrix <- IDBacApp::collapseSmallMolReplicates(checkedPool = checkedPool,
                                                   sampleIDs = matrixIDs ,
                                                   peakPercentPresence = peakPercentPresence,
                                                   lowerMassCutoff =lowerMassCutoff,
                                                   upperMassCutoff = upperMassCutoff, 
                                                   minSNR = minSNR)
    
    
  } else {
    matrix <- NULL
  }
  
  
  
  qwer <<-samples
  
  
  # Return pool
  pool::poolReturn(checkedPool)
  
  return(list(samplePeaks = reactive(samples),
              matrixPeaks = matrix))
  
  
}