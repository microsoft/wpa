# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Meeting Summary
#'
#' @description
#' Provides an overview analysis of weekly meeting hours.
#' Returns a bar plot showing average weekly meeting hours by default.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_bar
#' @inherit create_bar return
#'
#' @family Visualization
#' @family Meetings
#'
#' @examples
#' # Return a ggplot bar chart
#' meeting_summary(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' meeting_summary(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export
meeting_summary <- function(data,
                              hrvar = "Organization",
                              mingroup = 5,
                              return = "plot"){

  create_bar(data = data,
             metric = "Meeting_hours",
             hrvar = hrvar,
             mingroup = mingroup,
             return = return,
             bar_colour = "default")
}

#' @rdname meeting_summary
#' @export
meeting_sum <- meeting_summary







