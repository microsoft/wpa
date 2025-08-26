# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title External Collaboration Hours Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of External collaboration time, visualized as
#' line chart. By default returns a separate panel per value in the HR attribute. Additional
#' options available to return a summary table.
#'
#' @details
#' Uses the metric `Collaboration_hours_external`.
#'
#' @seealso [create_line()] for applying the same analysis to a different metric.
#'
#' @inheritParams create_line
#' @inherit create_line return
#'
#' @family Visualization
#' @family External Collaboration
#'
#' @examples
#' # Return a line plot
#' external_line(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' external_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

external_line <- function(data,
                            hrvar = "Organization",
                            mingroup=5,
                            return = "plot"){


plot_data <-  data %>% mutate(External_collaboration_hours = Collaboration_hours_external) 

  ## Inherit arguments
  create_line(data = plot_data,
              metric = "External_collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
