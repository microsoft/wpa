# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of
#' 'Weekly Digital Collaboration'. Returns a plot by default, with an option to
#' return a table with a all of groups (across multiple HR attributes) ranked by
#' hours of digital collaboration.
#'
#' @details
#' Uses the metric `Collaboration_hours`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#' @inherit create_rank return
#'
#' @family Visualization
#' @family Collaboration
#'
#' @examples
#' # Return rank table
#' collaboration_rank(
#'   data = sq_data,
#'   return = "table"
#' )
#'
#' # Return plot
#' collaboration_rank(
#'   data = sq_data,
#'   return = "plot"
#' )
#'
#'
#' @export

collaboration_rank <- function(data,
                               hrvar = extract_hr(data),
                               mingroup = 5,
                               mode = "simple",
                               plot_mode = 1,
                               return = "plot"){

  ## Handle variable name consistency
  data <- qui_stan_c(data)

  create_rank(data,
              metric = "Collaboration_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              mode = mode,
              plot_mode = plot_mode,
              return = return)

}

#' @rdname collaboration_rank
#' @export
collab_rank <- collaboration_rank
