% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_assembleMirrorPlots.R
\name{assembleMirrorPlots}
\alias{assembleMirrorPlots}
\title{Assemble mirror plot Data}
\usage{
assembleMirrorPlots(
  sampleID1,
  sampleID2,
  minFrequency,
  lowerMassCutoff,
  upperMassCutoff,
  minSNR,
  tolerance = 0.002,
  pool1,
  pool2,
  normalizeSpectra = FALSE,
  ...
)
}
\arguments{
\item{sampleID1}{sample ID to search in IDBac sqlite database (will be positive spectrum)}

\item{sampleID2}{sample ID to search in IDBac sqlite database (will be negative spectrum)}

\item{minFrequency}{numeric between 0 and 100, minFrequency}

\item{lowerMassCutoff}{lowerMassCutoff}

\item{upperMassCutoff}{upperMassCutoff}

\item{minSNR}{numeric, peaks with a SNR below this number will be removed}

\item{tolerance}{MALDIquant binning tolerance for intra-sample binning}

\item{pool1}{pool that contains sample 1 (positive spectrum)}

\item{pool2}{pool that contains sample 2 (negative spectrum)}

\item{normalizeSpectra}{should spectra be normalized? TRUE/FALSE}

\item{...}{advanced arguments for MALDIquant, see \code{\link[IDBacApp:normalizeSpectrumIntensity]{IDBacApp::normalizeSpectrumIntensity()}}}
}
\value{
environment containing mirror plot data
}
\description{
Assemble mirror plot Data
}
