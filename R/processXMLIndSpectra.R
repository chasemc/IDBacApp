
#' Process spectra data for input into SQLite
#'
#' @param spectraImport maldiquant spectra list
#' @param smallOrProtein "small" or "protein" chooses which processing pipeline is used
#' @param index logical vector describing which spectraImport spectra to use
#'
#' @return environment 
#' @export
#'
processXMLIndSpectra <- function(spectraImport,
                                 smallOrProtein,
                                 index){
  
  env <- new.env()
  
  env$massVector <- lapply(spectraImport[index], 
                           function(x){
                             x <- IDBacApp::serial(x@mass)
                             IDBacApp::chartoRawtoCompressed(x,
                                                             compression = 100)
                           })
  
  env$maxMass <- lapply(spectraImport[index], 
                    function(x){
                      max(x@mass)
                    })
  
  env$minMass <- lapply(spectraImport[index], 
                    function(x){
                      min(x@mass)
                    })
  
  # List of whole-specctrum hashes
  env$spectrumMassHash <- lapply(env$massVector, 
                                 function(x){
                                   IDBacApp::hashR(x)
                                 })
  
  env$spectrumIntensity <- lapply(spectraImport[index], 
                                  function(x){
                                    x <- IDBacApp::serial(x@intensity)
                                    IDBacApp::chartoRawtoCompressed(x,
                                                                    compression = 100)
                                  })
  
  env$spectrumIntensityHash <- lapply(env$spectrumIntensity, 
                                      function(x){
                                        IDBacApp::hashR(x)
                                      })
  
  
  if (smallOrProtein == "small") {
    env$peakMatrix <- IDBacApp::processSmallMolSpectra(spectraImport[index])
  } else if (smallOrProtein == "protein") {
    env$peakMatrix <- IDBacApp::processProteinSpectra(spectraImport[index])
  }
  
  if (!MALDIquant::isMassPeaksList(env$peakMatrix)) {
    env$peakMatrix <- list(env$peakMatrix)
  }
  
  env$peakMatrix <- lapply(env$peakMatrix, function(x){
    
    cbind(x@mass, x@intensity, x@snr)
    
  })
  
  
  
  
  return(env)
  
}