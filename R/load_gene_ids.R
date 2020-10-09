
#' Load Wormbase Gene ID table
#'
#' Load a cached geneIDs table. This gives the correspondence between gene symbol (e.g. unc-10),
#' Wormbase ID (e.g. WBGene00006750) and the sequence name (e.g. T10A3.1).
#'
#' @param WS Wormbase release version (e.g. WS277).
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return A tibble obtained from reading the Wormbase table.
#' @export
#'
#' @examples
#' gids <- load_gene_ids("WS277")
#' s2i("unc-10", gids)
load_gene_ids <- function(WS, dir_cache="~/../Projects/ref"){
  geneIDs <- readr::read_csv(paste0(dir_cache,.Platform$file.sep,
                                    "c_elegans.PRJNA13758.",WS,".geneIDs.txt.gz"),
                             col_names = c("X", "gene_id","symbol",
                                           "sequence","status","biotype"),
                             col_types = "cccccc")
  geneIDs$name <- ifelse(!is.na(geneIDs$symbol), geneIDs$symbol, geneIDs$sequence)
  geneIDs
}
