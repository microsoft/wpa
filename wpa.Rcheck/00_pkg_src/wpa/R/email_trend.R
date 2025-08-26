# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Email Hours Time Trend
#'
#' @description Provides a week by week view of email time.
#' By default returns a week by week heatmap, highlighting the points in time with most activity.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric `Email_hours`.
#'
#' @inheritParams create_trend
#' @inherit create_trend return
#'
#' @family Visualization
#' @family Emails
#'
#'
#' @export

email_trend <- function(data,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot"){

  create_trend(data,
               metric = "Email_hours",
               hrvar = hrvar,
               mingroup = mingroup,
               return = return)

}
