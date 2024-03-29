% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_networkFromDF.R
\name{modularityClustering}
\alias{modularityClustering}
\title{Color network based on fastgreedy.community}
\usage{
modularityClustering(igraphNetwork, hexColors = colorBlindPalette()[1:100])
}
\arguments{
\item{igraphNetwork}{igraph network}

\item{hexColors}{colors as hex}
}
\value{
colored igraph
}
\description{
Color network based on fastgreedy.community
}
