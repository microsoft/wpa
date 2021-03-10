# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Meeting Hours as a 100% stacked bar
#'
#' @description
#' Analyze Meeting Hours distribution.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_dist
#' @inherit create_dist return
#'
#' @family Visualization
#' @family Meetings
#'
#' @examples
#' # Return plot
#' meeting_dist(sq_data, hrvar = "Organization")
#'
#' # Return summary table
#' meeting_dist(sq_data, hrvar = "Organization", return = "table")
#'
#' # Return result with a custom specified breaks
#' meeting_dist(sq_data, hrvar = "LevelDesignation", cut = c(4, 7, 9))
#'
#' @export

meeting_dist <- function(data,
                         hrvar = "Organization",
                         mingroup = 5,
                         return = "plot",
                         cut = c(5, 10, 15)) {

  create_dist(data = data,
              metric = "Meeting_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)
}
