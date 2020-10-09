
load_gene_ids <- function(WS, dir_cache="~/../Projects/ref"){
  geneIDs <- readr::read_csv(paste0(dir_cache,.Platform$file.sep,
                                    "c_elegans.PRJNA13758.",WS,".geneIDs.txt.gz"),
                             col_names = c("X", "gene_id","symbol",
                                           "sequence","status","biotype"),
                             col_types = "cccccc")
  geneIDs$name <- ifelse(!is.na(geneIDs$symbol), geneIDs$symbol, geneIDs$sequence)
  geneIDs
}
