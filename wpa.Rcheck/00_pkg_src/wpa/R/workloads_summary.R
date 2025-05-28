# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Work Week Span Summary
#'
#' @description
#' Provides an overview analysis of 'Work Week Span'.
#' Returns a bar plot showing average weekly utilization hours by default.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_bar
#' @inherit create_bar return
#'
#' @family Visualization
#' @family Workweek Span
#'
#'
#' @examples
#' # Return a ggplot bar chart
#' workloads_summary(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' workloads_summary(sq_data, hrvar = "LevelDesignation", return = "table")
#' @export
workloads_summary <- function(data,
                              hrvar = "Organization",
                              mingroup = 5,
                              return = "plot"){

  create_bar(data = data,
             hrvar = hrvar,
             mingroup = mingroup,
             metric = "Workweek_span",
             return = return,
             bar_colour = "darkblue")
}


#' @rdname workloads_summary
#' @export
workloads_sum <- workloads_summary
