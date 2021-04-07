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
#' @details
#' Uses the metric `Meeting_hours_with_manager_1_on_1`.
#'
#' @inheritParams create_line
#' @inherit create_line return
#'
#' @family Visualization
#' @family Managerial Relations
#'
#' @examples
#' # Return a line plot
#' one2one_line(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' one2one_line(sq_data, hrvar = "LevelDesignation", return = "table")
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
