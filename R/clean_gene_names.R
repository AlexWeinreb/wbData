

#' Load the gene name history
#'
#' @param dir_cache Directory where the downloaded files are cached.
#' @param refresh Number of days after which the downloaded list needs to be refreshed.
#'
#' @return A tibble with "Input" containing historical gene names, "Suggested Match" the suggested match, and other fields
wb_get_gene_name_history <- function(dir_cache = NULL, refresh = 20){

  # validate input
  dir_cache <- get_dir_cache(dir_cache)

  stopifnot(is.numeric(refresh))

  # check if file needs refresh
  file_path <- get_filename("gene_name_history")
  cached_file <- file.path(dir_cache, file_path["filename"])
  if(file.exists(cached_file)){
    file_age <- as.numeric(difftime(Sys.time(), file.mtime(cached_file), units = "days"))
  }

  if(! file.exists(cached_file) || file_age >= refresh){
    file_url <- paste(file_path, collapse = "")
    utils::download.file(file_url, cached_file)
  }


  readr::read_tsv(cached_file, col_types = "cccccccccc", progress = FALSE)
}



#' Clean up old gene names
#'
#' @param gene_id Vector of gene IDs to clean.
#' @param warn_missing Warn if some gene IDs are not found.
#' @param dir_cache Directory where the downloaded files are cached.
#' @param refresh Number of days after which the downloaded list needs to be refreshed.
#' @param return_one if `FALSE`, a single input may return several gene IDs separated by commas when the gene was split. If set to `TRUE`, only the first match is returned.
#'
#' @description
#' Takes a list of (potentially old) Wormbase gene IDs, and replaces the Dead ones
#' with current IDs. This corresponds to the [Gene Name Sanitizer](https://wormbase.org/tools/mine/gene_sanitizer.cgi)
#' available on Wormbase.
#'
#' Note this function only works with gene IDs (of the form WBGene00000001), not
#' symbols or other sequence names, while the online tool can potentially correct
#' old gene names. The file downloaded by the (unexported) function `wb_get_gene_name_history()`
#' contains additional information, ask if you need this functionality to be added to the package.
#'
#'
#'
#' @return An updated list of gene IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' gids <- wb_load_gene_ids(294)
#' genes_of_interest <- c("WBGene00012733", "WBGene00000424", "WBGene00007566", "WBGene00045409")
#' i2s(genes_of_interest, gids, warn_missing = TRUE)
#' cleaned_genes <- wb_clean_gene_names(genes_of_interest)
#' i2s(cleaned_genes, gids, warn_missing = TRUE)
#' }
wb_clean_gene_names <- function(gene_id, warn_missing = TRUE, dir_cache = NULL, refresh = 20, return_one = FALSE){
  history_file <- wb_get_gene_name_history(dir_cache, refresh)

  not_found <- ( ! gene_id %in% history_file[["Input"]] )

  if(all(not_found)){
    stop("Gene ids not found")
  }


  res <- history_file$`Suggested Match`[match(gene_id, history_file$Input,
                                       incomparables = NA)]



  if(any(not_found) && warn_missing){

    gene_id <- gene_id[! not_found]

    warning(sum(not_found), " gene(s) not found. NAs will be returned")
  }

  res_na_when_id_found <- is.na(res[! not_found])

  if( any(res_na_when_id_found) && warn_missing ){

    warning(sum(res_na_when_id_found), " gene(s) obsolete, no update available. NAs returned")
  }

  if(return_one){
    res <- strsplit(res, split = ", ", fixed = TRUE) |>
      vapply(\(x) x[[1]],
             FUN.VALUE = character(1L))
  }

  res

}

