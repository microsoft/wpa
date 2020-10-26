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
#' @param return Character string with values "names" (default)
#' or "vars" to determine whether the function returns a character
#' vector of variable names or a data frame containing the HR
#' attributes.
#'
#' @family General
#'
#' @examples
#' sq_data %>% extract_hr(return = "names")
#'
#' sq_data %>% extract_hr(return = "vars")
#'
#' @export
extract_hr <- function(data,
                       max_unique = 50,
                       return = "names"){

  hr_var <-
    data %>%
    dplyr::select_if(~(is.character(.) | is.logical(.) | is.factor(.))) %>%
    dplyr::select_if(~(dplyr::n_distinct(.) < max_unique)) %>%
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



