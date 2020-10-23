#' Load Wormbase Gene ID table
#'
#' Load a geneIDs table. This gives the correspondence between gene symbol
#' (e.g. unc-10), Wormbase ID (e.g. WBGene00006750) and the sequence
#' name (e.g. T10A3.1). If the required geneIDs table is not cached,
#' it is first downloaded.
#'
#' @param WS Wormbase release version (e.g. WS277).
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return A tibble obtained from reading the Wormbase table.
#' @export
#'
#' @examples
#' gids <- wb_load_gene_ids(277)
#' s2i("unc-10", gids)
wb_load_gene_ids <- function(WS, dir_cache = NULL){

  # validate input
  if(is.character(WS)) WS <- get_WS(WS)
  dir_cache <- get_dir_cache(dir_cache)

  file_path <- get_filename("geneID", WS)
  cached_file <- file.path(dir_cache, file_path["filename"])
  if(! file.exists(cached_file)){
    ftp_path <- paste(file_path, collapse = "")
    utils::download.file(ftp_path, cached_file)
  }


  # read data, note format changed between versions
  if(WS >= 264){
    geneIDs <- readr::read_csv(cached_file,
                               col_names = c("X", "gene_id","symbol",
                                             "sequence","status","biotype"),
                               col_types = "cccccc")
  } else if(WS >= 236){
    geneIDs <- readr::read_csv(cached_file,
                               col_names = c("X", "gene_id","symbol",
                                             "sequence","status"),
                               col_types = "ccccc")
  } else{
    geneIDs <- readr::read_csv(cached_file,
                               col_names = c("gene_id","symbol","sequence"),
                               col_types = "ccc")
  }

  geneIDs$name <- ifelse(!is.na(geneIDs$symbol), geneIDs$symbol, geneIDs$sequence)
  geneIDs
}
