#' @title Collaboration Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of collaboration time, visualised as line charts.
#' By default returns a line chart for collaboration hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `Collaboration_hours`.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#'  but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Collaboration
#'
#' @examples
#'
#' ## Return a line plot
#' collaboration_line(sq_data, hrvar = "LevelDesignation")
#'
#'
#' ## Return a table
#' collaboration_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

collaboration_line <- function(data,
                                hrvar = "Organization",
                                mingroup=5,
                                return = "plot"){

  ## Inherit arguments
  output <- create_line(data = data,
              metric = "Collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

  if(return == "plot"){
    output +
      labs(title = "Collaboration Hours")
  } else if(return == "table"){
    output
  } else {
    stop("Invalid `return` value")
  }
}
