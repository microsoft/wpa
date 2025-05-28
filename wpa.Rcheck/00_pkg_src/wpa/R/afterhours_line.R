# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title After-hours Collaboration Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of after-hours collaboration time, visualized as
#' line charts. By default returns a line chart for after-hours collaboration
#' hours, with a separate panel per value in the HR attribute. Additional
#' options available to return a summary table.
#'
#' @details
#' Uses the metric `After_hours_collaboration_hours`.
#'
#' @seealso [create_line()] for applying the same analysis to a different metric.
#'
#' @inheritParams create_line
#' @inherit create_line return
#'
#' @family Visualization
#' @family After-hours Collaboration
#'
#' @examples
#' # Return a line plot
#' afterhours_line(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' afterhours_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

afterhours_line <- function(data,
                            hrvar = "Organization",
                            mingroup=5,
                            return = "plot"){

  ## Inherit arguments
  create_line(data = data,
              metric = "After_hours_collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
