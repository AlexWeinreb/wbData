% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s2i.R
\name{s2i}
\alias{s2i}
\title{Convert symbol to Wormbase ID}
\usage{
s2i(symbol, geneIDs, warn_missing = FALSE)
}
\arguments{
\item{symbol}{gene symbols.}

\item{geneIDs}{Translation table generated with `load_gene_ids()`.}

\item{warn_missing}{If TRUE, a warning is issued when a gene symbol was not found.}
}
\value{
The corresponding Wormbase IDs.
}
\description{
Convert symbol to Wormbase ID
}
\examples{
gids <- wb_load_gene_ids("WS277")
s2i("unc-10", gids)
}
