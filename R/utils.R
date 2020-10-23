#' Determine cache directory
#'
#' This internal function determines what directory should be used to store cache
#' files. It can be a user-provided path, set as a global option, or determined
#' on an OS basis.
#' @param dir_cache User-specified cache directory.
#'
#' @return The location of the cache directory, either explicitely user-provided,
#' specified as a global option, or (by default) in the OS cache directory.
#'
get_dir_cache <- function(dir_cache){
  if(is.null(dir_cache)){
    if(!is.null(getOption("wb_dir_cache"))){
      dir_cache <- getOption("wb_dir_cache")
    } else{
      dir_cache <- rappdirs::user_cache_dir("wbData", "wbData")
    }
  }

  if(!dir.exists(dir_cache)) dir.create(dir_cache, recursive = TRUE)

  dir_cache
}





#' Validate Wormbase release number.
#'
#' This internal function enables other functions to receive the Wormbase release
#' number as a numerical value or a character.
#'
#' @param WS Release number to validate.
#'
#' @return A number labeling a valid Wormbase release.
#'
get_WS <- function(WS){

  if(length(WS) > 1){
    warning("WS is of length > 1, only the first element is used.")
    WS <- WS[[1]]
  }
  if(grepl("^[0-2][0-9]{2}$", WS)){
    WS <- as.numeric(WS)
  } else if(grepl("^WS[0-2][0-9]{2}$", WS)){
    WS <- as.numeric(sub("WS","", WS))
  } else{
    stop("Wormbase release not recognized.")
  }

  WS
}


#' Clean wbData cache directory.
#'
#' The package downloads files from Wormbase when needed. They are stored
#' in a cache directory so as to alleviate the need for downloading the same file
#' every time it is needed. Use this function to clean up the cache directory when
#' a set of downloaded files will no longer be needed.
#'
#' @param WS If specified, only delete files corresponding to this Wormbase release.
#' @param dir_cache Location of the cache directory to clean up.
#' @param delete If FALSE, the list of files to delete is returned and nothing is actually deleted.
#'
#' @return Invisibly returns the cache directory location (if delete is TRUE).
#' @export
#'
#' @examples
#' dir <- tempdir()
#' wb_load_gene_ids(230, dir)
#' list.files(dir)
#' wb_clean_cache(dir_cache = dir)
#' list.files(dir)
wb_clean_cache <- function(WS = NULL, dir_cache = NULL, delete = TRUE){

  dir_cache <- get_dir_cache(dir_cache)

  cached_files <- list.files(dir_cache, full.names = TRUE)
  if(is.null(WS)){
    files_to_delete <- cached_files
  } else{
    files_to_delete <- cached_files[grepl(paste0("WS",WS),
                                          cached_files)]
  }

  if(! delete){
    return(files_to_delete)
  }

  file.remove(files_to_delete)
  invisible(dir_cache)
}
