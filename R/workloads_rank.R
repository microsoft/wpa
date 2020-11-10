# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Rank all groups across HR attributes for Work Week Span
#'
#' @description
#' This function scans a standard query output for groups with high levels of Work Week Span.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by work week span.
#'
#' @details
#' Uses the metric `Workweek_span`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#'
#' @family Workloads
#'
#' @return
#' When 'table' is passed in `return`, a summary table is returned as a data frame.
#'
#' @export

workloads_rank <- function(data,
                           hrvar = extract_hr(data),
                           mingroup = 5,
                           return = "table"){

  data %>%
    create_rank(metric = "Workweek_span",
                hrvar = hrvar,
                mingroup = mingroup,
                return = return)
}
