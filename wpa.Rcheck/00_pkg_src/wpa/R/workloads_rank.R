# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Rank all groups across HR attributes for Work Week Span
#'
#' @description
#' This function scans a standard query output for groups with high levels of
#' Work Week Span. Returns a plot by default, with an option to return a table
#' with a all of groups (across multiple HR attributes) ranked by work week
#' span.
#'
#' @details
#' Uses the metric `Workweek_span`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#' @inherit create_rank return
#'
#' @family Visualization
#' @family Workweek Span
#'
#' @examples
#' # Return rank table
#' workloads_rank(
#'   data = sq_data,
#'   return = "table"
#' )
#'
#' # Return plot
#' workloads_rank(
#'   data = sq_data,
#'   return = "plot"
#' )
#'
#' @export

workloads_rank <- function(data,
                           hrvar = extract_hr(data),
                           mingroup = 5,
                           mode = "simple",
                           plot_mode = 1,
                           return = "table"){

  data %>%
    create_rank(metric = "Workweek_span",
                hrvar = hrvar,
                mingroup = mingroup,
                mode = mode,
                plot_mode = plot_mode,
                return = return)
}
