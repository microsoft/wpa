# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Collaboration Hours as a 100% stacked bar
#'
#' @description
#' Analyze the distribution of Collaboration Hours.
#' Returns a stacked bar plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @template ch
#'
#' @inheritParams create_dist
#' @inherit create_dist return
#'
#' @family Visualization
#' @family Collaboration
#'
#' @examples
#' # Return plot
#' collaboration_dist(sq_data, hrvar = "Organization")
#'
#' # Return summary table
#' collaboration_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

collaboration_dist <- function(data,
                               hrvar = "Organization",
                               mingroup = 5,
                               return = "plot",
                               cut = c(15, 20, 25)) {

  ## Handle variable name consistency
  data <- qui_stan_c(data)

  create_dist(data = data,
              metric = "Collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return,
              cut = cut)


}

#' @rdname collaboration_dist
#' @export
collab_dist <- collaboration_dist
