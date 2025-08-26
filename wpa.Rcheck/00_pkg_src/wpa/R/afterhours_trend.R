# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title After-Hours Time Trend
#'
#' @description
#' Provides a week by week view of after-hours collaboration time.
#' By default returns a week by week heatmap, highlighting the points in time with most activity.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `After_hours_collaboration_hours`.
#'
#' @inheritParams create_trend
#'
#' @family Visualization
#' @family After-hours Collaboration
#'
#' @examples
#' # Run plot
#' afterhours_trend(sq_data)
#'
#' # Run table
#' afterhours_trend(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a 'ggplot' object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

afterhours_trend <- function(data,
                             hrvar = "Organization",
                             mingroup = 5,
                             return = "plot"){

  create_trend(data,
               metric = "After_hours_collaboration_hours",
               hrvar = hrvar,
               mingroup = mingroup,
               return = return)

}
