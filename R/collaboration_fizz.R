# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Collaboration Hours (Fizzy Drink plot)
#'
#' @description
#' Analyze weekly collaboration hours distribution, and returns
#' a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' Uses the metric `Collaboration_hours`.
#'
#' @inheritParams create_fizz
#' @inherit create_fizz return
#'
#' @family Collaboration
#'
#' @examples
#' # Return plot
#' collaboration_fizz(sq_data, hrvar = "Organization", return = "plot")
#'
#' # Return summary table
#' collaboration_fizz(sq_data, hrvar = "Organization", return = "table")
#'
#' @export

collaboration_fizz <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){

  create_fizz(data = data,
              metric = "Collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)

}

#' @rdname collaboration_fizz
#' @export
collab_fizz <- collaboration_fizz
