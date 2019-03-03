
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
                                                             compression = 0)
                           })
  
  env$maxMass <- unlist(lapply(spectraImport[index], 
                    function(x){
                      as.integer(max(x@mass))
                    }))
  
  env$minMass <- unlist(lapply(spectraImport[index], 
                    function(x){
                      as.integer(min(x@mass))
                    }))
  
  # List of whole-spectrum hashes
  env$spectrumMassHash <- unlist(lapply(env$massVector, 
                                 function(x){
                                   IDBacApp::hashR(x)
                                 }))
  
  env$spectrumIntensity <- lapply(spectraImport[index], 
                                  function(x){
                                    x <- IDBacApp::serial(x@intensity)
                                    IDBacApp::chartoRawtoCompressed(x,
                                                                    compression = 0)
                                  })
  
  env$spectrumIntensityHash <- unlist(lapply(env$spectrumIntensity, 
                                      function(x){
                                        IDBacApp::hashR(x)
                                      }))
  
  
  if (smallOrProtein == "small") {
    env$peakMatrix <- IDBacApp::processSmallMolSpectra(spectraImport[index])
  } else if (smallOrProtein == "protein") {
    env$peakMatrix <- IDBacApp::processProteinSpectra(spectraImport[index])
  }
  
  if (!MALDIquant::isMassPeaksList(env$peakMatrix)) {
    env$peakMatrix <- list(env$peakMatrix)
  }
  
  env$peakMatrix <- lapply(env$peakMatrix, function(x){
    # note: names aren't transferred to JSON
    cbind(mass = x@mass, 
          intensity = x@intensity, 
          snr = x@snr)
    
  })
  
  env$peakMatrix <- unlist(lapply(env$peakMatrix, 
                           function(x)
                             as.character(IDBacApp::serial(x))))
  
  
  
  
  
  return(env)
  
}