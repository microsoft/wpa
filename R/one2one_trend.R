# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Manager 1:1 Time Trend
#'
#' @description
#' Provides a week by week view of scheduled manager 1:1 Time.
#' By defualt returns a week by week heatmap, highlighting the points in time with most activity.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `Meeting_hours_with_manager_1_on_1`.
#'
#' @inheritParams create_trend
#'
#' @family Managerial Relations
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

one2one_trend <- function(data,
                          hrvar = "Organization",
                          mingroup = 5,
                          return = "plot"){

  create_trend(data,
               metric = "Meeting_hours_with_manager_1_on_1",
               hrvar = hrvar,
               mingroup = mingroup,
               return = return)

}





