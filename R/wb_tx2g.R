#' Convert between transcript ID and gene ID
#'
#' @param tx transcript IDs (e.g. MTCE.10).
#' @param gene_id Wormbase gene ID (e.g. WBGene00000001).
#' @param tx2g_tab Translation table generated with `wb_load_tx2gene()`.
#' @param warn_missing If TRUE, a warning is issued when an ID was not found.
#' @param simplify If TRUE, attempts to simplify the results into a vector.
#'
#' @details These functions convert between different transcript and gene identifiers using a preloaded
#'       translation table.
#'
#'       Note that a gene can have several matching transcripts.
#'
#'
#' @return The corresponding IDs, as a character vector for \code{wb_tx2g}, as a list for \code{wb_g2tx}. If `simplify = TRUE`,
#' attempts to cast the result into a vector. This is not possible if the input genes do not have the same number of transcripts.
#' @export
#'
#' @examples
#' \dontrun{
#'   wb_tx2g(c("Y37A1A.28", "W10C8.12", "Y4C6A.2c", "F56D6.15", "D1086.11a.1"), tx2g_tab)
#'   wb_g2tx(c("WBGene00017981", "WBGene00045419", "mistake"), tx2g_tab, warn_missing = TRUE)
#' }
wb_tx2g <- function(tx, tx2g_tab, warn_missing = FALSE){
  res <- tx2g_tab$gene_id[match(tx, tx2g_tab$transcript_id, incomparables = NA_character_)]
  if(warn_missing && any(is.na(res))){
    warning("tx2g: ",sum(is.na(res))," transcript IDs could not be converted. NA are returned.")
  }
  res
}

#' @rdname wb_tx2g
#' @export
wb_g2tx <- function(gene_id, tx2g_tab, warn_missing = FALSE, simplify = FALSE){
  res <- sapply(gene_id,
                \(.g) {
                  tx_rows <- which(tx2g_tab$gene_id == .g)
                  if(length(tx_rows) == 0) return(NA_character_)
                  tx2g_tab$transcript_id[tx_rows]
                  },
                simplify = simplify)

  if(warn_missing && any(is.na(res))){
    warning("g2tx: ",sum(is.na(res))," gene IDs could not be converted. NA are returned.")
  }
  res
}
