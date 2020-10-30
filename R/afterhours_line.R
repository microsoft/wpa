#' @title After-hours Collaboration Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of after-hours collaboration time, visualized as line charts.
#' By default returns a line chart for after-hours collaboration hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `After_hours_collaboration_hours`.
#' See `create_line()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_line
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family After-Hours
#'
#' @examples
#'
#' ## Return a line plot
#' afterhours_line(sq_data, hrvar = "LevelDesignation")
#'
#'
#' ## Return a table
#' afterhours_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

afterhours_line <- function(data,
                            hrvar = "Organization",
                            mingroup=5,
                            return = "plot"){

  ## Inherit arguments
  output <- create_line(data = data,
              metric = "After_hours_collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

  if(return == "plot"){
    output +
      labs(title = "After-hours collaboration Hours")
  } else if(return == "table"){
    output
  } else {
    stop("Invalid `return` value")
  }
}
