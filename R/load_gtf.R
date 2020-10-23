#' Load gene coordinates
#'
#' Loads a data frame providing the genomic coordinates of all genes, built from
#' the Wormbase GTF file. The GTF file will be downloaded as needed.
#'
#' @param WS Wormbase release version.
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return A tibble obtained from reading the Wormbase GTF, containing the
#' following fields:
#' \itemize{
#'   \item \code{gene_id}: Wormbase gene ID (WBGene).
#'   \item \code{chr}: Chromosome name (I, II, III, IV, V, X, MtDNA).
#'   \item \code{start}, \code{end}, \code{strand}: Genomic position of the gene.
#'   \item \code{position}: Genomic position (compatible with JBrowse or IGV).
#'   \item \code{gene_biotype}: Gene biotype (e.g. protein_coding or tRNA)
#' }
#' @export
#'
#' @examples
#' \dontrun{
#'   gene_coords <- wb_load_gene_coords("WS273")
#'   gene_coords[gene_coords$gene_id == "WBGene00004323", "position"]
#' }

wb_load_gene_coords <- function(WS, dir_cache = NULL){

  # validate input
  if(is.character(WS)) WS <- get_WS(WS)
  dir_cache <- get_dir_cache(dir_cache)

  file_path <- get_filename("gtf", WS)
  cached_file <- file.path(dir_cache, file_path["filename"])
  if(! file.exists(cached_file)){
    ftp_path <- paste(file_path, collapse = "")
    utils::download.file(ftp_path, cached_file)
  }


  # read data
  full_gtf <- readr::read_tsv(cached_file,
                            skip=1,
                            col_names = c("chr","source", "feature",
                                          "start", "end", "score", "strand",
                                          "frame", "attributes"),
                            col_types = "cccddcccc")

  gene_coords <- full_gtf[full_gtf$feature == "gene",c(1, 4:5, 7, 9)]
  attrs <- gene_coords$attributes

  if(WS == 268){
    # WS268 has a single entry with 'Gene:' in the gene id
    attrs <- sub("Gene:WBGene00021498", "WBGene00021498", attrs)
  }

  # define regex to extract gene_id and biotype
  if(WS == 253){
    # WS253 has gene symbol as additional field for protein-coding genes
    # also, no trailing ';'
    gene_regex <- "^gene_id \"(WBGene\\d{8})\"; gene_biotype \"(\\w+)\"( ; locus \"[\\w\\-\\.]+\")?$"
  } else if(WS >= 262 && WS <= 268){
    warning("For WS262-WS268 the miRNA gene IDs are non-standard. Further processing may be required.")
    gene_regex <- "^gene_id \"(WBGene\\d{8}|Transcript:\\w{3,7}\\.\\d{1,4})\"; gene_source \"WormBase\"; gene_biotype \"(\\w+)\";$"
  } else{
    gene_regex <- "^gene_id \"(WBGene\\d{8})\"; gene_source \"WormBase\"; gene_biotype \"(\\w+)\";$"
  }


  m <- regexec(gene_regex, attrs, perl=TRUE)

  gene_coords$gene_id <- vapply(regmatches(attrs,m), function(x) x[[2]],
                                character(1))
  gene_coords$gene_biotype <- vapply(regmatches(attrs,m), function(x) x[[3]],
                                     character(1))
  gene_coords$position <- paste0(gene_coords$chr,":",
                                 format(gene_coords$start,big.mark = ","),
                                 "-",
                                 format(gene_coords$end, big.mark = ","))
  gene_coords$attributes <- NULL



  gene_coords[c(5,1:4,7:6)]
}







#' Load exon coordinates
#'
#' Loads a data frame providing the genomic coordinates of all exons, built from
#' the Wormbase GTF file. The GTF file will be downloaded as needed.
#'
#' @param WS Wormbase release version.
#' @param dir_cache Directory where the downloaded files are cached.
#'
#' @return A tibble obtained from reading the Wormbase GTF, containing the
#' following fields:
#' \itemize{
#'   \item \code{exon_id}: Unique exon ID, composed of the transcript name
#'   with the exon number.
#'   \item \code{transcript_id}: Transcript ID, typically the sequence name,
#'   an isoform letter (if relevant) and a transcript number (if relevant).
#'   \item \code{gene_id}: Wormbase gene ID (WBGene).
#'   \item \code{exon_number}: An integer identifying exons of each transcript.
#'   \item \code{chr}: Chromosome name (I, II, III, IV, V, X, MtDNA).
#'   \item \code{start}, \code{end}, \code{strand}: Genomic position of the exon.
#'   \item \code{gene_biotype}: Gene biotype (e.g. protein_coding or tRNA).
#'   \item \code{transcript_biotype}: Trtanscript biotype, usually the same as the gene biotype.
#'   \item \code{position}: Genomic position (compatible with JBrowse or IGV).
#' }
#' @export
#'
#' @examples
#' exon_coords <- wb_load_exon_coords("WS277")
#' exon_coords[exon_coords$transcript_id == "K08H10.7.2", "position"]
wb_load_exon_coords <- function(WS, dir_cache = NULL){

  # validate input
  if(is.character(WS)) WS <- get_WS(WS)
  dir_cache <- get_dir_cache(dir_cache)

  file_path <- get_filename("gtf", WS)
  cached_file <- file.path(dir_cache, file_path["filename"])
  if(! file.exists(cached_file)){
    ftp_path <- paste(file_path, collapse = "")
    utils::download.file(ftp_path, cached_file)
  }


  # read data
  full_gtf <- readr::read_tsv(cached_file,
                              skip=1,
                              col_names = c("chr","source", "feature",
                                            "start", "end", "score", "strand",
                                            "frame", "attributes"),
                              col_types = "cccddcccc")

  exon_coords <- full_gtf[full_gtf$feature == "exon",c(1, 4:5, 7, 9)]
  attrs <- exon_coords$attributes

  if(WS == 268){
    # WS268 has a single entry with 'Gene:' in the gene id
    attrs <- sub("Gene:WBGene00021498", "WBGene00021498", attrs)
  }

  # pattern for exon
  if(WS == 253){
    warning("For WS253, only the gene and transcript ID are available.")
    ex_pat <- "^gene_id \"(WBGene\\d{8})\" ; transcript_id \"([\\w\\.]{3,20})\""
  } else if(WS >= 262 && WS <= 268){
    warning("For WS262-WS268 the miRNA gene IDs are non-standard. Further processing may be required.")
    ex_pat <- "^gene_id \"(WBGene\\d{8}|Transcript:\\w{3,7}\\.\\d{1,4})\"; transcript_id \"([\\w\\.]{3,20})\"; exon_number \"(\\d{1,2})\"; gene_source \"WormBase\"; gene_biotype \"(\\w+)\"; transcript_source \"WormBase\"; transcript_biotype \"(\\w+)\"; exon_id \"([\\w\\.]{3,22})\";$"
  } else{
    ex_pat <- "^gene_id \"(WBGene\\d{8})\"; transcript_id \"([\\w\\.]{3,20})\"; exon_number \"(\\d{1,2})\"; gene_source \"WormBase\"; gene_biotype \"(\\w+)\"; transcript_source \"WormBase\"; transcript_biotype \"(\\w+)\"; exon_id \"([\\w\\.]{3,22})\";$"
  }



  m <- regexec(ex_pat, attrs, perl=TRUE)

  exon_coords$gene_id <- vapply(regmatches(attrs,m), function(x) x[[2]],
                                character(1))
  cat("............")
  exon_coords$transcript_id <- vapply(regmatches(attrs,m), function(x) x[[3]],
                                character(1))
  cat("..........")
  if(WS > 253){
    exon_coords$exon_number <- as.integer(vapply(regmatches(attrs,m), function(x) x[[4]],
                                                 character(1)))
    cat("...........")
    exon_coords$gene_biotype <- vapply(regmatches(attrs,m), function(x) x[[5]],
                                       character(1))
    cat("..........")
    exon_coords$transcript_biotype <- vapply(regmatches(attrs,m), function(x) x[[6]],
                                             character(1))
    cat("...........")
    exon_coords$exon_id <- vapply(regmatches(attrs,m), function(x) x[[7]],
                                  character(1))
    cat("...........")
  }

  exon_coords$position <- paste0(exon_coords$chr,":",
                                 format(exon_coords$start,big.mark = ","),
                                 "-",
                                 format(exon_coords$end, big.mark = ","))
  cat("...........\n")
  exon_coords$attributes <- NULL


  if(WS == 253) return(exon_coords[c(6,5,1:4,7)])

  exon_coords[c(10,6,5,7,1:4,8:9,11)]
}