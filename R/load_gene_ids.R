#' Get FTP path and filename of the geneIDs table
#'
#' This internal function gets the Wormbase FTP location of the filename
#' of the geneIDs table for a given Wormbase release.
#'
#' @param WS Wormbase release number.
#'
#' @return a vector of length 2 giving the FTP path and the filename.
#'
get_filename_geneID <- function(WS){
  if(WS >= 237){
    path <- c(path=paste0("ftp://ftp.wormbase.org/pub/wormbase/releases/WS",
                   WS,"/species/c_elegans/PRJNA13758/annotation/"),
              filename=paste0("c_elegans.PRJNA13758.WS",WS,".geneIDs.txt.gz"))
  } else if(WS >= 226){
    path <- c(path=paste0("ftp://ftp.wormbase.org/pub/wormbase/releases/WS",
                   WS,"/species/c_elegans/annotation/"),
              filename=paste0("c_elegans.WS",WS,".geneIDs.txt.gz"))
  } else if(WS >= 197){
    path <- c(path=paste0("ftp://ftp.wormbase.org/pub/wormbase/releases/WS",
                   WS,"/species/c_elegans/annotation/"),
              filename=paste0("geneIDs.WS",WS,".gz"))
  } else if(WS <= 190){
    stop("Annotations not available for WS190 and older.")
  } else stop("Unrecognized Wormbase release.")

  path
}



#' Find the filename and FTP path
#'
#' For now, only geneID is a recognized file type. If needed, GTF or other types
#' of data could be added here.
#'
#' @param type Type of file to obtain, e.g. geneID or GTF.
#' @param WS Wormbase release version.
#'
#' @return A length 2 vector containing the FTP path to the file and the name of the file.
get_filename <- function(type, WS){
  switch(tolower(type),
         "geneid"=, "geneids" = get_filename_geneID(WS),
         stop("Type of file not recognized.")
  )
}



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
