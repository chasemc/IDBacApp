#' Process spectra data for input into SQLite
#'
#' @param spectraImport maldiquant spectra list
#' @param smallOrProtein "small" or "protein" chooses which processing pipeline is used
#' @param index logical vector describing which spectraImport spectra to use
#' @param ... advanced arguments for MALDIquant, see [IDBacApp::processSmallMolSpectra()] and/or [IDBacApp::processProteinSpectra()]
#'
#' @return environment
#'
#'
processXMLIndSpectra <- function(spectraImport,
                                 smallOrProtein,
                                 index,
                                 ...) {
  env <- new.env()
  env$mass_vector <- lapply(
    spectraImport[index],
    function(x) {
      x <- serial(x@mass)
      chartoRawtoCompressed(x,
        compression = 0
      )
    }
  )
  env$max_mass <- unlist(lapply(
    spectraImport[index],
    function(x) {
      as.integer(max(x@mass))
    }
  ))
  env$min_mass <- unlist(lapply(
    spectraImport[index],
    function(x) {
      as.integer(min(x@mass))
    }
  ))
  # List of whole-spectrum hashes
  env$spectrum_mass_hash <- unlist(lapply(
    env$mass_vector,
    function(x) {
      hashR(x)
    }
  ))
  env$spectrum_intensity <- lapply(
    spectraImport[index],
    function(x) {
      x <- serial(x@intensity)
      chartoRawtoCompressed(x,
        compression = 0
      )
    }
  )
  env$spectrum_intensity_hash <- unlist(lapply(
    env$spectrum_intensity,
    function(x) {
      hashR(x)
    }
  ))

  if (smallOrProtein == "small") {
    env$peak_matrix <- processSmallMolSpectra(
      input = spectraImport[index],
      ...
    )
  } else if (smallOrProtein == "protein") {
    env$peak_matrix <- processProteinSpectra(
      input = spectraImport[index],
      ...
    )
  }
  if (!MALDIquant::isMassPeaksList(env$peak_matrix)) {
    env$peak_matrix <- list(env$peak_matrix)
  }
  env$peak_matrix <- lapply(env$peak_matrix, function(x) {
    # note: names aren't transferred to JSON
    list(
      mass = x@mass,
      intensity = x@intensity,
      snr = x@snr
    )
  })
  env$peak_matrix <- unlist(lapply(
    env$peak_matrix,
    function(x) {
      as.character(serial(x))
    }
  ))
  return(env)
}
