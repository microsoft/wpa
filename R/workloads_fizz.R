# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Work Week Span (Fizzy Drink plot)
#'
#' @description
#' Analyze Work Week Span distribution, and returns
#' a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_fizz
#' @inherit create_fizz return
#'
#' @family Workloads
#'
#' @examples
#' # Return plot
#' workloads_fizz(sq_data, hrvar = "Organization", return = "plot")
#'
#' # Return summary table
#' workloads_fizz(sq_data, hrvar = "Organization", return = "table")
#'
#' @export

workloads_fizz <- function(data,
                           hrvar = "Organization",
                           mingroup = 5,
                           return = "plot"){

  create_fizz(data = data,
              metric = "Workweek_span",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}
