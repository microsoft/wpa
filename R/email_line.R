# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Email Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of email time, visualised as line charts.
#' By default returns a line chart for email hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_line
#' @inherit create_line return
#'
#' @family Visualization
#' @family Emails
#'
#' @examples
#' # Return a line plot
#' email_line(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' email_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export
email_line <- function(data,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot"){
  ## Inherit arguments
  create_line(data = data,
              metric = "Email_hours",
              hrvar = hrvar,
              mingroup = mingroup,
              return = return)
}
