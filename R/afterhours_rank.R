#' @title Rank groups with high After-Hours Collaboration Hours
#'
#' @description
#' This function scans a Standard Person Query for groups with high levels of After-Hours Collaboration.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by hours of After-Hours Collaboration Hours.
#'
#' @details
#' Uses the metric \code{After_hours_collaboration_hours}.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param hrvar A list of HR Variables to consider in the scan.
#' Defaults to all HR attributes identified.
#' @param mingroup Numeric value setting the privacy threshold / minimum group size.
#' Defaults to 5.
#' @param return A character vector specifying what to return.
#' Valid values include "html" (default, returning an interactive DataTable)
#' and "df" (data frame)
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family After-Hours
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

afterhours_rank <- function(data,
                            hrvar = extract_hr(data),
                            mingroup = 5,
                            return = "table"){

  output <-
    data %>%
    create_rank(metric = "After_hours_collaboration_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                return = "table")

  if(return == "html"){
    return(create_dt(output))
  } else if(return == "table"){
    return(output)
  } else {
    stop("Invalid `return` argument.")
  }
}
