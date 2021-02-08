# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Workloads Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of 'Work Week Span', visualised as line charts.
#' By default returns a line chart for collaboration hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_line
#' @inherit create_line return
#'
#' @family Workloads
#'
#' @examples
#' # Return a line plot
#' workloads_line(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' workloads_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

workloads_line <- function(data,
                           hrvar = "Organization",
                           mingroup=5,
                           return = "plot"){

  ## Inherit arguments
  output <- create_line(data = data,
                        metric = "Workweek_span",
                        hrvar = hrvar,
                        mingroup = mingroup,
                        return = return)

}
