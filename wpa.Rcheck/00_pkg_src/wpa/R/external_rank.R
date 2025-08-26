# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Rank groups with high External Collaboration Hours
#'
#' @description
#' This function scans a Standard Person Query for groups with high levels of
#' External Collaboration. Returns a plot by default, with an option to
#' return a table with all groups (across multiple HR attributes) ranked by
#' hours of External Collaboration.
#'
#' @details
#' Uses the metric \code{Collaboration_hours_external}.
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
#' @family Visualization
#' @family After-hours Collaboration
#'
#' @return
#' When 'table' is passed in `return`, a summary table is returned as a data frame.
#'
#' @export

external_rank <- function(data,
                            hrvar = extract_hr(data),
                            mingroup = 5,
                            mode = "simple",
                            plot_mode = 1,
                            return = "plot"){

plot_data <-  data %>% mutate(External_collaboration_hours = Collaboration_hours_external) 
 
  
  plot_data %>%
    create_rank(metric = "External_collaboration_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                mode = mode,
                plot_mode = plot_mode,
                return = return)
}
