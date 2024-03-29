% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_processMZML.R
\name{db_from_mzml}
\alias{db_from_mzml}
\title{db_from_mzml}
\usage{
db_from_mzml(mzFilePaths, sampleIds, idbacPool, acquisitionInfo, ...)
}
\arguments{
\item{mzFilePaths}{file paths of the mzml files}

\item{sampleIds}{sampleIds that are in the same order as the paths}

\item{idbacPool}{idbac single pool, not a list of pools}

\item{acquisitionInfo}{acquisitionInfo (currently only used when converting from Bruker raw data)}

\item{...}{advanced arguments for MALDIquant, see \code{\link[IDBacApp:processSmallMolSpectra]{IDBacApp::processSmallMolSpectra()}} and/or \code{\link[IDBacApp:processProteinSpectra]{IDBacApp::processProteinSpectra()}}}
}
\value{
Nothing direct, creates a sqlite database
}
\description{
db_from_mzml
}
