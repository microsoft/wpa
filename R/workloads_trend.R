# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Work Week Span Time Trend
#'
#' @description
#' Provides a week by week view of Work Week Span.
#' By default returns a week by week heatmap, highlighting the points in time with most activity.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `Workweek_span`.
#'
#' @inheritParams create_trend
#'
#' @family Workloads
#'
#' @examples
#' # Run plot
#' workloads_trend(sq_data)
#'
#' # Run table
#' workloads_trend(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

workloads_trend <- function(data,
                            hrvar = "Organization",
                            mingroup = 5,
                            return = "plot"){

  create_trend(data,
               metric = "Workweek_span",
               hrvar = hrvar,
               mingroup = mingroup,
               return = return)

}





