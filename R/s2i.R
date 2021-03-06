
#' Convert symbol to Wormbase ID
#'
#' @param symbol gene symbols.
#' @param geneIDs Translation table generated with `load_gene_ids()`.
#' @param warn_missing If TRUE, a warning is issued when a gene symbol was not found.
#'
#' @return The corresponding Wormbase IDs.
#' @export
#'
#' @examples
#' gids <- wb_load_gene_ids("WS277")
#' s2i("unc-10", gids)
s2i <- function(symbol, geneIDs, warn_missing = FALSE){
  res <- geneIDs$gene_id[match(symbol, geneIDs$name)]
  if(warn_missing && any(is.na(res))){
    warning("s2i: ",sum(is.na(res))," gene symbols could not be converted. NA are returned.")
  }
  res
}


#' Convert Wormbase ID to symbol
#'
#' @param gene_id Wormbase gene IDs
#' @param geneIDs Translation table generated by `load_gene_ids()`.
#' @param warn_missing If TRUE, a warning is issued when a gene ID was not found.
#'
#' @return The corresponding gene symbols. If a gene symbol is missing, the sequence
#' name is returned instead.
#' @export
#'
#' @examples
#' gids <- wb_load_gene_ids("WS277")
#' s2i("unc-10", gids)
i2s <- function(gene_id, geneIDs, warn_missing = FALSE){
  res <- geneIDs$name[match(gene_id, geneIDs$gene_id)]
  if(warn_missing && any(is.na(res))){
    warning("i2s: ",sum(is.na(res))," gene IDs could not be converted. NA are returned.")
  }
  res
}
