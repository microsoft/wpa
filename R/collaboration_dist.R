# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration Hours distribution
#'
#' @description
#' Analyze the distribution of Collaboration Hours.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' Uses the metric `Collaboration_hours`.
#'
#' @inheritParams create_dist
#' @family Collaboration
#'
#' @examples
#' ## Return a plot
#' collaboration_dist(sq_data, hrvar = "Organization")
#'
#' ## Return a table
#' collaboration_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

collaboration_dist <- function(data,
                               hrvar = "Organization",
                               mingroup = 5,
                               return = "plot",
                               cut = c(15, 20, 25)) {

  create_dist(data = data,
              metric = "Collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)


}

#' @rdname collaboration_dist
#' @export
collaboration_distribution <- collaboration_dist
