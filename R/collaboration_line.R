# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of collaboration time, visualised as line charts.
#' By default returns a line chart for collaboration hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @template ch
#'
#' @inheritParams create_line
#' @inherit create_line return
#'
#' @family Visualization
#' @family Collaboration
#'
#' @examples
#' # Return a line plot
#' collaboration_line(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' collaboration_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

collaboration_line <- function(data,
                                hrvar = "Organization",
                                mingroup = 5,
                                return = "plot"){

  ## Handle variable name consistency
  data <- qui_stan_c(data)

  ## Inherit arguments
  create_line(data = data,
              metric = "Collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)
}

#' @rdname collaboration_line
#' @export
collab_line <- collaboration_line
