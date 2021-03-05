# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Meeting Hours Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of
#' Weekly Meeting Collaboration. Returns a table with a all of groups (across
#' multiple HR attributes) ranked by hours of digital collaboration.
#'
#' @details
#' Uses the metric `Meeting_hours`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#' @inherit create_rank return
#'
#' @family Visualization
#' @family Meetings
#'
#'
#' @export

meeting_rank <- function(data,
                         hrvar = extract_hr(data),
                         mingroup = 5,
                         return = "table"){
  data %>%
    create_rank(metric = "Meeting_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                return = return)

}
