# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Extract HR attribute variables
#'
#' @description
#' This function uses a combination of variable class,
#' number of unique values, and regular expression matching
#' to extract HR / organisational attributes from a data frame.
#'
#' @param data A data frame to be passed through.
#' @param max_unique A numeric value representing the maximum
#' number of unique values to accept for an HR attribute. Defaults to 50.
#' @param exclude_constants Logical value to specify whether single-value HR
#' attributes are to be excluded. Defaults to `TRUE`.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"names"`
#'   - `"vars"`
#'
#' See `Value` for more information.

#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"names"`: character vector identifying all the names of HR variables
#'   present in the data.
#'   - `"vars"`: data frame containing all the columns of HR variables present
#'    in the data.
#'
#' @family Support
#' @family Data Validation
#'
#' @examples
#' sq_data %>% extract_hr(return = "names")
#'
#' sq_data %>% extract_hr(return = "vars")
#'
#' @export
extract_hr <- function(data,
                       max_unique = 50,
                       exclude_constants = TRUE,
                       return = "names"){


  if(exclude_constants == TRUE){

    min_unique = 1

  } else if (exclude_constants == FALSE){

    min_unique = 0

  }


  hr_var <-
    data %>%
    dplyr::select_if(~(is.character(.) | is.logical(.) | is.factor(.))) %>%
    dplyr::select_if(~(dplyr::n_distinct(.) < max_unique)) %>%
    dplyr::select_if(~(dplyr::n_distinct(.) > min_unique)) %>% # Exc constants
    dplyr::select_if(~!all(is_date_format(.))) %>%
    names() %>%
    .[.!= "WorkingStartTimeSetInOutlook"] %>%
    .[.!= "WorkingEndTimeSetInOutlook"] %>%
    .[.!= "WorkingDaysSetInOutlook"]

  if(return == "names"){
    return(hr_var)
  } else if(return == "vars"){
    return(dplyr::select(data, tidyselect::all_of(hr_var)))
  } else {
   stop("Invalid input for return")
  }
}



