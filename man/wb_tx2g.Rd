% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wb_tx2g.R
\name{wb_tx2g}
\alias{wb_tx2g}
\alias{wb_g2tx}
\title{Convert between transcript ID and gene ID}
\usage{
wb_tx2g(tx, tx2g_tab, warn_missing = FALSE)

wb_g2tx(gene_id, tx2g_tab, warn_missing = FALSE, simplify = FALSE)
}
\arguments{
\item{tx}{transcript IDs (e.g. MTCE.10).}

\item{tx2g_tab}{Translation table generated with `wb_load_tx2gene()`.}

\item{warn_missing}{If TRUE, a warning is issued when an ID was not found.}

\item{gene_id}{Wormbase gene ID (e.g. WBGene00000001).}

\item{simplify}{If TRUE, attempts to simplify the results into a vector.}
}
\value{
The corresponding IDs, as a character vector for \code{wb_tx2g}, as a list for \code{wb_g2tx}. If `simplify = TRUE`,
attempts to cast the result into a vector. This is not possible if the input genes do not have the same number of transcripts.
}
\description{
Convert between transcript ID and gene ID
}
\details{
These functions convert between different transcript and gene identifiers using a preloaded
      translation table.

      Note that a gene can have several matching transcripts.
}
\examples{
\dontrun{
  wb_tx2g(c("Y37A1A.28", "W10C8.12", "Y4C6A.2c", "F56D6.15", "D1086.11a.1"), tx2g_tab)
  wb_g2tx(c("WBGene00017981", "WBGene00045419", "mistake"), tx2g_tab, warn_missing = TRUE)
}
}
