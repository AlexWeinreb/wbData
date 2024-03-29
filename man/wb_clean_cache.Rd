% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{wb_clean_cache}
\alias{wb_clean_cache}
\title{Clean wbData cache directory.}
\usage{
wb_clean_cache(WS = NULL, dir_cache = NULL, delete = TRUE)
}
\arguments{
\item{WS}{If specified, only delete files corresponding to this Wormbase release.}

\item{dir_cache}{Location of the cache directory to clean up.}

\item{delete}{If FALSE, the list of files to delete is returned and nothing is actually deleted.}
}
\value{
Invisibly returns the cache directory location (if delete is TRUE).
}
\description{
The package downloads files from Wormbase when needed. They are stored
in a cache directory so as to alleviate the need for downloading the same file
every time it is needed. Use this function to clean up the cache directory when
a set of downloaded files will no longer be needed.
}
\examples{
\dontrun{
dir <- tempdir()
wb_load_gene_ids(230, dir)
list.files(dir)
wb_clean_cache(dir_cache = dir)
list.files(dir)
}
}
