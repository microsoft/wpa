#' @title Generate a time stamp
#'
#' @description
#' This function generates a time stamp of the format yymmdd_hhmmss.
#' This is a support function and is not intended for direct use.
#'
#'
#' @export
tstamp <- function(){
  stamp <- Sys.time()
  stamp <- gsub(pattern = "[[:punct:]]", replacement = "", x = stamp)
  stamp <- gsub(pattern = " ", replacement = "_", x = stamp)

  return(stamp)
}
