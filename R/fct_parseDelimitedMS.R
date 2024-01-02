#' Parse Delimited MS files with fread and MALDIquant
#'
#' @param proteinPaths proteinPaths
#' @param proteinNames proteinNames
#' @param smallMolPaths smallMolPaths
#' @param smallMolNames smallMolNames names of the
#' @param centroid Do the files contain profile or centroid data (currently implemented as just
#'     one TRUE/FALSE, but could mappply)
#' @param exportDirectory Where will mzml files be written?
#'
#' @return sample-named vector of filpaths of random-named mzml files
#' @export
#'
delim_to_mzml <- function(proteinPaths,
                          proteinNames,
                          smallMolPaths,
                          smallMolNames,
                          exportDirectory,
                          centroid) {
  if (length(proteinPaths) != length(proteinNames)) {
    stop("unequal proteinPaths and proteinNames")
  }
  if (length(smallMolPaths) != length(smallMolNames)) {
    stop("unequal proteinPaths and proteinNames")
  }

  combPaths <- c(
    proteinPaths,
    smallMolPaths
  )
  combPaths <- unlist(combPaths)
  combNames <- c(
    proteinNames,
    smallMolNames
  )
  combNames <- unlist(combNames)
  # Read delim files, for now just regular mass then intensity columns
  importedFiles <- lapply(
    combPaths,
    function(x) {
      a <- data.table::fread(x)
      MALDIquant::createMassSpectrum(
        mass = a[[1]],
        intensity = a[[2]]
      )
    }
  )

  mzFilePaths <- sapply(
    seq_along(unique(combNames)),
    function(x) {
      paste0(tempfile(tmpdir = exportDirectory),
        fileext = ".mzml"
      )
    }
  )
  mzFilePaths <- normalizePath(mzFilePaths,
    mustWork = FALSE,
    winslash = "/"
  )

  key <- base::split(importedFiles, combNames)
  names(mzFilePaths) <- names(key)
  lengthProgress <- length(key)
  # withProgress doesn't currently work outside shiny
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    withProgress(
      message = "Conversion in progress",
      detail = "This may take a while...",
      value = 0,
      {
        for (i in seq_along(key)) {
          incProgress(1 / lengthProgress)
          MALDIquantForeign::exportMzMl(
            x = as.list(key[[i]]),
            path = mzFilePaths[[i]],
            force = TRUE
          )
        }
      }
    )
  } else {
    for (i in seq_along(key)) {
      MALDIquantForeign::exportMzMl(
        x = as.list(key[[i]]),
        path = mzFilePaths[[i]],
        force = TRUE
      )
    }
  }

  return(mzFilePaths)
}
