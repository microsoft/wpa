# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Email Hours Ranking
#'
#' @description
#' This function scans a standard query output for groups with high levels of
#' 'Weekly Email Collaboration'. Returns a plot by default, with an option to
#' return a table with a all of groups (across multiple HR attributes) ranked by
#' hours of digital collaboration.
#'
#' @details
#' Uses the metric `Email_hours`.
#' See `create_rank()` for applying the same analysis to a different metric.
#'
#' @inheritParams create_rank
#' @inherit create_rank return
#'
#' @family Visualization
#' @family Emails
#'
#' @examples
#' # Return rank table
#' email_rank(
#'   data = sq_data,
#'   return = "table"
#' )
#'
#' # Return plot
#' email_rank(
#'   data = sq_data,
#'   return = "plot"
#' )
#'
#' @export

email_rank <- function(data,
                       hrvar = extract_hr(data),
                       mingroup = 5,
                       mode = "simple",
                       plot_mode = 1,
                       return = "plot"){

  data %>%
    create_rank(metric = "Email_hours",
                hrvar = hrvar,
                mingroup = mingroup,
                mode = mode,
                plot_mode = plot_mode,
                return = return)

}
