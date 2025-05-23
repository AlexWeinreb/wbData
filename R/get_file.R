get_url_root <- function(protocol = c("https", "ftp")){
  protocol <- match.arg(protocol)

  if(protocol == "ftp"){
    root <- "ftp://ftp.wormbase.org/pub/wormbase/releases/"
  } else {
    root <- "https://downloads.wormbase.org/releases/"
  }

  root
}



#' Get FTP path and filename of the geneIDs table
#'
#' This internal function gets the Wormbase FTP location of the filename
#' of the geneIDs table for a given Wormbase release.
#'
#' @param WS Wormbase release number.
#' @param protocol use https or ftp
#'
#' @return a vector of length 2 giving the FTP path and the filename.
#'
get_filename_geneID <- function(WS, protocol = "https"){

  base_url <- get_url_root(protocol)

  if(WS >= 237){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/PRJNA13758/annotation/"),
              filename=paste0("c_elegans.PRJNA13758.WS",WS,".geneIDs.txt.gz"))

  } else if(WS >= 226){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/annotation/"),
              filename=paste0("c_elegans.WS",WS,".geneIDs.txt.gz"))

  } else if(WS >= 197){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/annotation/"),
              filename=paste0("geneIDs.WS",WS,".gz"))

  } else if(WS %in% seq(130, 190, 10)){
    path <- c(path=paste0(base_url, "WS", WS,"/"),
              filename=paste0("geneIDs.WS",WS,".gz"))

  } else if(WS < 197){
    stop("Annotations not available for WS196 and older (except 130, 140, 150, 160, 170, 180, 190).")

  } else stop("Unrecognized Wormbase release.")

  path
}


#' Get FTP path and filename of the GTF file
#'
#' This internal function gets the Wormbase FTP location and the filename
#' of the GTF file for a given Wormbase release.
#'
#' @param WS Wormbase release number.
#' @param protocol use https or ftp
#'
#' @return a vector of length 2 giving the FTP path and the filename.
#'
get_filename_gtf <- function(WS, protocol = "https"){

  base_url <- get_url_root(protocol)

  if(WS >= 253){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/PRJNA13758/"),
              filename=paste0("c_elegans.PRJNA13758.WS",WS,".canonical_geneset.gtf.gz"))

  } else if(WS <= 252){
    stop("GTF not available for WS252 and older.")
  } else stop("Unrecognized Wormbase release.")

  path
}


#' Get filename for a TxDb file
#'
#' This internal function sets the name to use to store a TxDb file in cache.
#'
#' @param WS Wormbase release number.
#'
#' @return a vector of length 1 with the filename.
#'
get_filename_txdb <- function(WS){
  c(filename=paste0("c_elegans.PRJNA13758.WS",WS,".txdb.sqlite"))
}


#' Get FTP path and filename of the genome FASTA file
#'
#' This internal function gets the Wormbase FTP location and the filename
#' of the genome FASTA file for a given Wormbase release.
#'
#' @param WS Wormbase release number.
#' @param protocol use https or ftp
#'
#' @return a vector of length 2 giving the FTP path and the filename.
#'
get_filename_genome <- function(WS, protocol = "https"){

  base_url <- get_url_root(protocol)

  if(WS >= 237){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/PRJNA13758/"),
              filename=paste0("c_elegans.PRJNA13758.WS",WS,".genomic.fa.gz"))

  } else if(WS >= 197){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/"),
              filename=paste0("c_elegans.WS",WS,".genomic.fa.gz"))

  } else if(WS %in% seq(100, 190, 10)){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/"),
              filename=paste0("c_elegans.WS",WS,".genomic.fa.gz"))

  } else if(WS == 77){
    path <- c(path=paste0(base_url, "WS077/species/c_elegans/"),
              filename=paste0("c_elegans.WS077.genomic.fa.gz"))

  } else if(WS < 197){
    stop("Genome not available for WS196 and older (except 077 and multiples of 10, e.g. 100, 130, 190).")

  } else stop("Unrecognized Wormbase release.")

  path
}

#' Get FTP path and filename of the transcriptome FASTA file
#'
#' This internal function gets the Wormbase FTP location and the filename
#' of the transcriptome FASTA file for a given Wormbase release.
#'
#' @param WS Wormbase release number.
#' @param protocol use https or ftp
#'
#' @return a vector of length 2 giving the FTP path and the filename. Note the filename changes between versions!
#'
get_filename_transcriptome <- function(WS, protocol = "https"){

  base_url <- get_url_root(protocol)

  if(WS >= 244){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/PRJNA13758/"),
              filename=paste0("c_elegans.PRJNA13758.WS",WS,".mRNA_transcripts.fa.gz"))

  } else if(WS >= 237){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/PRJNA13758/"),
              filename=paste0("c_elegans.WS",WS,".coding_transcripts.fa.gz"))

  }  else if(WS == 236){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/"),
              filename=paste0("c_elegans.WS",WS,".coding_transcripts.fa.gz"))

  } else if(WS >= 197){

    warning("Full transcripts not available before WS236, downloading CDS transcripts instead (no UTR).")

    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/"),
              filename=paste0("c_elegans.WS",WS,".cds_transcripts.fa.gz"))

  } else if(WS %in% seq(100, 190, 10)){
    path <- c(path=paste0(base_url, "WS", WS,
                          "/species/c_elegans/"),
              filename=paste0("c_elegans.WS",WS,".cds_transcripts.fa.gz"))

  } else if(WS < 197){

    stop("Transcriptome not available for WS196 and older (except multiples of 10, e.g. 100, 130, 190).")

  } else stop("Unrecognized Wormbase release.")

  path
}


#' Get URL of CGC strain list
#'
#'
#' @return a vector of length 2 giving the URL path and the filename.
get_filename_cgc <- function(){
  c(path="https://cgc.umn.edu/static/",
    filename="cgc-strains.txt")
}


get_filename_gene_name_history <- function(){
  c(path="http://caltech.wormbase.org/pub/wormbase/spell_download/tables/",
    filename="GeneNameHistory.csv")
}


#' Find the filename and FTP path
#'
#' Dispatch to the corresponding file type function.
#'
#' @param type Type of file to obtain, e.g. geneID or GTF.
#' @param WS Wormbase release version number.
#'
#' @return A length 2 vector containing the FTP path to the file and the name of the file.
get_filename <- function(type, WS){
  switch(tolower(type),
         "geneid"=, "geneids" = get_filename_geneID(WS),
         "gtf" = get_filename_gtf(WS),
         "txdb" = get_filename_txdb(WS),
         "genome" = get_filename_genome(WS),
         "transcriptome" = get_filename_transcriptome(WS),
         "cgc" = get_filename_cgc(),
         "gene_name_history" = get_filename_gene_name_history(),
         stop("Type of file not recognized.")
  )
}
