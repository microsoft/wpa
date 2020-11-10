# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of 'Weekly Digital Collaboration'.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by hours of digital collaboration.
#'
#' @details
#' Uses the metric `Collaboration_hours`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#'
#' @family Collaboration
#'
#' @return
#' When 'table' is passed in `return`, a summary table is returned as a data frame.
#'
#' @export

collaboration_rank <- function(data,
                               hrvar = extract_hr(data),
                               mingroup = 5,
                               return = "table"){

  data %>%
    create_rank(metric="Collaboration_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                return = return)

}
