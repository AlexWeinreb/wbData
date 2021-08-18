#' Load the list of strains available at the CGC
#'
#' @param dir_cache Directory where the downloaded files are cached.
#' @param refresh Number of days after which the downloaded list needs to be refreshed.
#'
#' @return A tibble containing the list of strains available at the CGC. Description of the fields can be found on the CGC website.
#' @export
#'
#' @examples
#' \dontrun{
#' strain_list <- wb_load_cgc_list()
#' nrow(strain_list)
#' strain_list$Genotype[strain_list$Strain == "NC902"]
#' }
wb_load_cgc_list <- function(dir_cache = NULL, refresh = 2){

  # validate input
  dir_cache <- get_dir_cache(dir_cache)

  stopifnot(is.numeric(refresh))

  # check if file needs refresh
  file_path <- get_filename("cgc")
  cached_file <- file.path(dir_cache, file_path["filename"])
  if(file.exists(cached_file)){
    file_age <- as.numeric(difftime(Sys.time(), file.mtime(cached_file), units = "days"))
  }

  if(! file.exists(cached_file) || file_age >= refresh){
    cgc_url <- paste(file_path, collapse = "")
    utils::download.file(cgc_url, cached_file)
  }


  raw_list <- readr::read_lines(cached_file)

  strainlist <- split(raw_list,
                      cumsum(grepl("^ +-+ $", raw_list)))
  if(length(strainlist[[length(strainlist)]]) <= 3){
    # remove the last entry which is often blank
    strainlist <- strainlist[-length(strainlist)]
  }

  strainlist_collapsed <- lapply(strainlist, function(x) paste(x, collapse = ""))


  full_pattern <- "(Strain): (.*)(Species): (.*)(Genotype): (.*)(Description): (.*)(Mutagen): (.*)(Outcrossed): (.*)(Made by): (.*)(Received): (.*)"

  matches <- regmatches(strainlist_collapsed,
                         m = regexec(full_pattern, strainlist_collapsed))

  matches <- lapply(matches,
               function(m) stats::setNames(m[c(3, 5, 7, 9, 11, 13, 15, 17)],
                                    m[c(2,4,6,8,10,12,14,16)]))

  strains_df <- dplyr::bind_rows(matches)
  strains_df <- dplyr::mutate(strains_df,
                               dplyr::across(dplyr::everything(), trimws))

  strains_df
}



