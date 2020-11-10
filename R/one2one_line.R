# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Manager 1:1 Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of 1:1 time with managers, visualised as line charts.
#' By default returns a line chart for 1:1 meeting hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
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
#' @family Managerial Relations
#'
#' @examples
#'
#' ## Return a line plot
#' one2one_line(sq_data, hrvar = "LevelDesignation")
#'
#'
#' ## Return a table
#' one2one_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
one2one_line <- function(data,
                         hrvar = "Organization",
                         mingroup = 5,
                         return = "plot"){
  ## Inherit arguments
  create_line(data = data,
              metric = "Meeting_hours_with_manager_1_on_1",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)
}
