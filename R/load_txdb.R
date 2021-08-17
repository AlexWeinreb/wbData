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
