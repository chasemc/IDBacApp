% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_exportmzML.R
\name{exportmzML}
\alias{exportmzML}
\title{Export mzML/mzXML from SQLite DB}
\usage{
exportmzML(pool, sampleIDs, saveToDir)
}
\arguments{
\item{pool}{pool connection, not-checked-out}

\item{sampleIDs}{sample IDs}

\item{saveToDir}{directory files should be saved to}
}
\value{
NA, files written
}
\description{
Export mzML/mzXML from SQLite DB
}
