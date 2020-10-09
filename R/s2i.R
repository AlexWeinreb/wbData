
s2i <- function(symbol, geneIDs) return(geneIDs$gene_id[match(symbol, geneIDs$symbol)])
i2s <- function(gene_id, geneIDs) return(geneIDs$name[match(gene_id, geneIDs$gene_id)])
