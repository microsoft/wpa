# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of After-hours Collaboration Hours (Fizzy Drink plot)
#'
#' @description
#' Analyze weekly after-hours collaboration hours distribution, and returns
#' a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' Uses the metric `After_hours_collaboration_hours`.
#' See `create_fizz()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_fizz
#' @inherit create_fizz return
#'
#' @family Visualization
#' @family After-hours Collaboration
#'
#' @examples
#' # Return plot
#' afterhours_fizz(sq_data, hrvar = "LevelDesignation", return = "plot")
#'
#' # Return summary table
#' afterhours_fizz(sq_data, hrvar = "Organization", return = "table")
#' @export

afterhours_fizz <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){

  create_fizz(data = data,
              metric = "After_hours_collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
