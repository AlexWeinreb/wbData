% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_dir_cache}
\alias{get_dir_cache}
\title{Determine cache directory}
\usage{
get_dir_cache(dir_cache)
}
\arguments{
\item{dir_cache}{User-specified cache directory.}
}
\value{
The location of the cache directory, either explicitely user-provided,
specified as a global option, or (by default) in the OS cache directory.
}
\description{
This internal function determines what directory should be used to store cache
files. It can be a user-provided path, set as a global option, or determined
on an OS basis.
}
