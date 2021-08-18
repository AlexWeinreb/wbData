
#' Convert between gene symbol, Wormbase ID, and sequence name
#'
#' @param symbol gene symbols (e.g. aap-1).
#' @param gene_id Wormbase gene ID (e.g. WBGene00000001).
#' @param seq_id sequence ID (e.g. F27C8.1)
#' @param geneIDs Translation table generated with `load_gene_ids()`.
#' @param warn_missing If TRUE, a warning is issued when a gene symbol was not found.
#'
#' @details These functions convert between different gene identifiers using a preloaded
#'       translation table. `s2i` and `i2s` are the most commonly used, a longer name would
#'       be `wb_symbol2id` and `wb_id2symbol`. The gene `name` is the gene `symbol` if it exists,
#'       and the sequence ID otherwise.
#'
#'
#' @return The corresponding IDs.
#' @export
#'
#' @examples
#' gids <- wb_load_gene_ids("WS277")
#' s2i(c("unc-10", "aap-1"), gids)
s2i <- function(symbol, geneIDs, warn_missing = FALSE){
  res <- geneIDs$gene_id[match(symbol, geneIDs$name)]
  if(warn_missing && any(is.na(res))){
    warning("s2i: ",sum(is.na(res))," gene symbols could not be converted. NA are returned.")
  }
  res
}


#' @rdname s2i
#' @export
i2s <- function(gene_id, geneIDs, warn_missing = FALSE){
  res <- geneIDs$name[match(gene_id, geneIDs$gene_id)]
  if(warn_missing && any(is.na(res))){
    warning("i2s: ",sum(is.na(res))," gene IDs could not be converted. NA are returned.")
  }
  res
}

#' @rdname s2i
#' @export
wb_seq2id <- function(seq_id, geneIDs, warn_missing = FALSE){
  res <- geneIDs$gene_id[match(seq_id, geneIDs$sequence)]
  if(warn_missing && any(is.na(res))){
    warning("wb_seq2symbol: ",sum(is.na(res))," sequence IDs could not be converted. NA are returned.")
  }
  res
}

#' @rdname s2i
#' @export
wb_id2seq <- function(gene_id, geneIDs, warn_missing = FALSE){
  res <- geneIDs$sequence[match(gene_id, geneIDs$gene_id)]
  if(warn_missing && any(is.na(res))){
    warning("i2s: ",sum(is.na(res))," gene IDs could not be converted. NA are returned.")
  }
  res
}

#' @rdname s2i
#' @export
wb_seq2name <- function(seq_id, geneIDs, warn_missing = FALSE){
  res <- geneIDs$name[match(seq_id, geneIDs$sequence)]
  if(warn_missing && any(is.na(res))){
    warning("wb_seq2name: ",sum(is.na(res))," sequence IDs could not be converted. NA are returned.")
  }
  res
}

#' @rdname s2i
#' @export
wb_symbol2seq <- function(symbol, geneIDs, warn_missing = FALSE){
  res <- geneIDs$sequence[match(symbol, geneIDs$symbol)]
  if(warn_missing && any(is.na(res))){
    warning("wb_name2seq: ",sum(is.na(res))," gene names could not be converted. NA are returned.")
  }
  res
}
