#' Load TxDb
#'
#' Generate and load a TxDb object for a given Wormbase version, that can be used with Bioconductor packages.
#'
#' @param WS Wormbase release version.
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return A TxDb object.
#' @export
#'
#' @examples
#' \dontrun{
#' wb_txdb_274 <- wb_load_TxDb(274)
#' transcript_to_gene_table <- AnnotationDbi::select(wb_txdb_274,
#'                                          keys = keys(wb_txdb_274, keytype = "TXNAME"),
#'                                          columns = c("TXNAME", "GENEID"),
#'                                          keytype = "TXNAME")
#' all_exons_GRanges <- GenomicFeatures::exonsBy(wb_txdb_274, by = "gene")
#' }
#'
wb_load_TxDb <- function(WS, dir_cache = NULL){

  if(! requireNamespace("GenomicFeatures", quietly=TRUE)){
    stop('To create a TxDb you need to install GenomicFeatures.\n Run BiocManager::install("GenomicFeatures") or see https://www.bioconductor.org/packages/release/bioc/html/GenomicFeatures.html for details')
  }

  # validate input
  if(is.character(WS)) WS <- get_WS(WS)
  dir_cache <- get_dir_cache(dir_cache)

  file_path <- get_filename("txdb", WS)
  cached_file <- file.path(dir_cache, file_path["filename"])

  if(! file.exists(cached_file)){
    gtf_path <- wb_get_gtf_path(WS, dir_cache)
    txdb <- GenomicFeatures::makeTxDbFromGFF(gtf_path,
                                             organism = "Caenorhabditis elegans",
                                             dataSource = "WormBase")
    AnnotationDbi::saveDb(txdb, file.path(dir_cache, file_path["filename"]))
  } else{
    txdb <- AnnotationDbi::loadDb(file.path(dir_cache, file_path["filename"]))
  }

  txdb
}
