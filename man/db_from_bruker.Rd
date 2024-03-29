% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_convertBruker.R
\name{db_from_bruker}
\alias{db_from_bruker}
\title{Convert Bruker raw fids to IDBac database}
\usage{
db_from_bruker(
  dataDirectory = NULL,
  fileName = NULL,
  filePath = NULL,
  anyMissing = NULL,
  acquisitionInfo = NULL,
  sampleMap = NULL,
  tempDir = NULL,
  ...
)
}
\arguments{
\item{dataDirectory}{path to directory containing fids (warning: does a recursive path search for fids)}

\item{fileName}{name of IDBac SQLite file}

\item{filePath}{path where IDBac SQLite file will be written}

\item{anyMissing}{advanced, mainly here to be used by the GUI code}

\item{acquisitionInfo}{advanced, mainly here to be used by the GUI code}

\item{sampleMap}{matrix or df representing the MALDI target plate (you can replace values in matrix created with IDBacApp:::map384Well())}

\item{tempDir}{temporary folder with read/write permission to write intermediate mzml files to}

\item{...}{advanced arguments for MALDIquant, see \code{\link[IDBacApp:processSmallMolSpectra]{IDBacApp::processSmallMolSpectra()}} and/or \code{\link[IDBacApp:processProteinSpectra]{IDBacApp::processProteinSpectra()}}}
}
\value{
none, side effect of writing SQLite  file
}
\description{
Convert Bruker raw fids to IDBac database
}
