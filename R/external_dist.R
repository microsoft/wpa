# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Email Hours as a 100% stacked bar
#'
#' @description
#' Analyze External Collaboration Hours distribution.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_dist
#' @inherit create_dist return
#'
#' @family Visualization
#' @family External
#'
#' @examples
#' # Return plot
#' external_dist(sq_data, hrvar = "Organization")
#'
#' # Return summary table
#' external_dist(sq_data, hrvar = "Organization", return = "table")
#'
#' # Return result with a custom specified breaks
#' external_dist(sq_data, hrvar = "LevelDesignation", cut = c(4, 7, 9))
#'
#' @export

external_dist <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot",
                       cut = c(15, 20, 25)) {

  create_dist(data = data,
              metric = "Collaboration_hours_external",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)
}
