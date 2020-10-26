#' @title Identify whether string is a date format
#'
#' @description
#' This function uses regular expression to determine whether
#' a string is of the format "mdy", separated by "-", "/",
#' or ".", returning a logical vector.
#'
#' @param string Character string to test whether is a date format.
#'
#' @examples
#' \dontrun{
#' is_date_format("1/5/2020")
#' }
#'
#' @export
is_date_format <- function(string){
  grepl("^\\d{1,2}[- /.]\\d{1,2}[- /.]\\d{1,4}$",
        string)
}
