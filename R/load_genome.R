#' Get path to cached genome file
#'
#' Returns the path to a genome FASTA file in the cache. Useful to use with functions that require
#' a connection to read from, or to pass to other packages. The file gets
#' downloaded as necessary.
#'
#' @param WS Wormbase release version.
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return a character string pointing to a "c_elegans.PRJNA13758.WSxxx.genomic.fa.gz" file.
#' @export
#'
wb_get_genome_path <- function(WS, dir_cache = NULL){

  # validate input
  if(is.character(WS)) WS <- get_WS(WS)
  dir_cache <- get_dir_cache(dir_cache)

  file_path <- get_filename("genome", WS)
  cached_file <- file.path(dir_cache, file_path["filename"])
  if(! file.exists(cached_file)){
    ftp_path <- paste(file_path, collapse = "")
    utils::download.file(ftp_path, cached_file)
  }

  # return path to file
  cached_file
}
