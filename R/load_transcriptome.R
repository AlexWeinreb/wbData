#' Get path to cached transcriptome file
#'
#' Returns the path to a transcripts FASTA file in the cache. Useful to use with functions that require
#' a connection to read from, or to pass to other packages. The file gets
#' downloaded as necessary.
#'
#' @param WS Wormbase release version.
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return a character string pointing to a "c_elegans.PRJNA13758.WSxxx.mRNA_transcripts.fa.gz" file.
#' @export
#'
wb_get_transcriptome_path <- function(WS, dir_cache = NULL){

  # validate input
  if(is.character(WS)) WS <- get_WS(WS)
  dir_cache <- get_dir_cache(dir_cache)

  file_path <- get_filename("transcriptome", WS)

  # note the filename on the ftp changes between versions, we can't rely on it
  cached_file <- file.path(dir_cache, paste0("c_elegans.PRJNA13758.WS",WS,".mRNA_transcripts.fa.gz"))

  if(! file.exists(cached_file)){
    ftp_path <- paste(file_path, collapse = "")
    utils::download.file(ftp_path, cached_file)
  }

  # return path to file
  cached_file
}
