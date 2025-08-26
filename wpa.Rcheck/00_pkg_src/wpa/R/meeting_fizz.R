# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Meeting Hours (Fizzy Drink plot)
#'
#' @description
#' Analyze weekly meeting hours distribution, and returns a 'fizzy' scatter plot
#' by default. Additional options available to return a table with distribution
#' elements.
#'
#' @details
#' Uses the metric `Meeting_hours`.
#'
#' @inheritParams create_fizz
#' @inherit create_fizz return
#'
#' @family Visualization
#' @family Meetings
#'
#' @examples
#' # Return plot
#' meeting_fizz(sq_data, hrvar = "Organization", return = "plot")
#'
#' # Return summary table
#' meeting_fizz(sq_data, hrvar = "Organization", return = "table")
#' @export

meeting_fizz <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){

  create_fizz(data = data,
              metric = "Meeting_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
