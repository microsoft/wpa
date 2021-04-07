#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param hrvar String containing the name of the HR Variable by which to split
#'   metrics. Defaults to `"Organization"`. To run the analysis on the total
#'   instead of splitting by an HR attribute, supply `NULL` (without quotes).
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
