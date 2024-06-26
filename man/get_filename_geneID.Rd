% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_file.R
\name{get_filename_geneID}
\alias{get_filename_geneID}
\title{Get FTP path and filename of the geneIDs table}
\usage{
get_filename_geneID(WS, protocol = "https")
}
\arguments{
\item{WS}{Wormbase release number.}

\item{protocol}{use https or ftp}
}
\value{
a vector of length 2 giving the FTP path and the filename.
}
\description{
This internal function gets the Wormbase FTP location of the filename
of the geneIDs table for a given Wormbase release.
}
