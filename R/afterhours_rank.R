# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Rank groups with high After-Hours Collaboration Hours
#'
#' @description
#' This function scans a Standard Person Query for groups with high levels of After-Hours Collaboration.
#' Returns a table with a all of groups (across multiple HR attributes) ranked by hours of After-Hours Collaboration Hours.
#'
#' @details
#' Uses the metric \code{After_hours_collaboration_hours}.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family After-Hours
#'
#' @return
#' When 'table' is passed in `return`, a summary table is returned as a data frame.
#'
#' @export

afterhours_rank <- function(data,
                            hrvar = extract_hr(data),
                            mingroup = 5,
                            return = "table"){

  data %>%
    create_rank(metric = "After_hours_collaboration_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                return = return)
}
