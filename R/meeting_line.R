# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Meeting Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of meeting time, visualised as line charts.
#' By default returns a line chart for meeting hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_line
#'
#' @family Meetings
#'
#' @examples
#'
#' ## Return a line plot
#' meeting_line(sq_data, hrvar = "LevelDesignation")
#'
#'
#' ## Return a table
#' meeting_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
meeting_line <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){
  ## Inherit arguments
  create_line(data = data,
              metric = "Meeting_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)
}
