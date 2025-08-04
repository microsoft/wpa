#' @title Replace underscore with space
#'
#' @description Convenience function to convert underscores to space
#'
#' @param x String to replace all occurrences of `_` with a single space
#'
#' @return
#' Character vector containing the modified string.
#'
#' @family Support
#'
#' @examples
#' us_to_space("Meeting_hours_with_manager_1_on_1")
#'
#' @export
us_to_space <- function(x){
  gsub(pattern = "_", replacement = " ", x = x)
}
