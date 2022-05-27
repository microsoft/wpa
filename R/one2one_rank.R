# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Manager 1:1 Time Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of
#' 'Manager 1:1 Time'. Returns a plot by default, with an option to return a
#' table with a all of groups (across multiple HR attributes) ranked by manager
#' 1:1 time.
#'
#' @details
#' Uses the metric `Meeting_hours_with_manager_1_on_1`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#' @inherit create_rank return
#'
#' @family Visualization
#' @family Managerial Relations
#'
#' @examples
#' # Return rank table
#' one2one_rank(
#'   data = sq_data,
#'   return = "table"
#' )
#'
#' # Return plot
#' one2one_rank(
#'   data = sq_data,
#'   return = "plot"
#' )
#'
#' @export

one2one_rank <- function(data,
                         hrvar = extract_hr(data),
                         mingroup = 5,
                         mode = "simple",
                         plot_mode = 1,
                         return = "plot"){

  data %>%
    create_rank(metric = "Meeting_hours_with_manager_1_on_1",
                hrvar = hrvar,
                mingroup = mingroup,
                mode = mode,
                plot_mode = plot_mode,
                return = return)
}
