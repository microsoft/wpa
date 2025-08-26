# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Fabricate a 'Total' HR variable
#'
#' @description Create a 'Total' column of character type comprising exactly of
#'   one unique value. This is a convenience function for returning a no-HR
#'   attribute view when `NULL` is supplied to the `hrvar` argument in
#'   functions.
#'
#' @examples
#' # Create a visual without HR attribute breaks
#' sq_data %>%
#'   totals_col() %>%
#'   collab_fizz(hrvar = "Total")
#'
#' @param data data frame
#' @param total_value Character value defining the name and the value of the
#'   `"Total"` column. Defaults to `"Total"`. An error is returned if an
#'   existing variable has the same name as the supplied value.
#'
#' @return
#' data frame containing an additional 'Total' column on top of the input data
#' frame.
#'
#' @family Support
#'
#' @export
totals_col <- function(data, total_value = "Total"){

  if(total_value %in% names(data)){
    stop(paste("Column", wrap(total_value, wrapper = "`"), "already exists. Please supply a different
               value to `total_value`"))
  }

  data %>%
    dplyr::mutate(!!sym(total_value) := total_value)
}
