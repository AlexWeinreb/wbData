% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_transcriptome.R
\name{wb_get_transcriptome_path}
\alias{wb_get_transcriptome_path}
\title{Get path to cached transcriptome file}
\usage{
wb_get_transcriptome_path(WS, dir_cache = NULL)
}
\arguments{
\item{WS}{Wormbase release version.}

\item{dir_cache}{Directory where the downloaded files are cached.}
}
\value{
a character string pointing to a "c_elegans.PRJNA13758.WSxxx.mRNA_transcripts.fa.gz" file.
}
\description{
Returns the path to a transcripts FASTA file in the cache. Useful to use with functions that require
a connection to read from, or to pass to other packages. The file gets
downloaded as necessary.
}
