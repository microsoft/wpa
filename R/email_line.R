#' @title Email Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of email time, visualised as line charts.
#' By default returns a line chart for email hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_line
#'
#' @family Emails
#'
#' @examples
#'
#' ## Return a line plot
#' email_line(sq_data, hrvar = "LevelDesignation")
#'
#'
#' ## Return a table
#' email_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
email_line <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){
  ## Inherit arguments
  create_line(data = data,
              metric = "Email_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)
}
