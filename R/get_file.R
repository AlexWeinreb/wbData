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


#' Get FTP path and filename of the GTF file
#'
#' This internal function gets the Wormbase FTP location and the filename
#' of the GTF file for a given Wormbase release.
#'
#' @param WS Wormbase release number.
#'
#' @return a vector of length 2 giving the FTP path and the filename.
#'
get_filename_gtf <- function(WS){
  if(WS >= 253){
    path <- c(path=paste0("ftp://ftp.wormbase.org/pub/wormbase/releases/WS",
                          WS,"/species/c_elegans/PRJNA13758/"),
              filename=paste0("c_elegans.PRJNA13758.WS",WS,".canonical_geneset.gtf.gz"))
  } else if(WS <= 252){
    stop("GTF not available for WS252 and older.")
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
         "cgc" = get_filename_cgc(),
         stop("Type of file not recognized.")
  )
}
