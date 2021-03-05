# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Email Summary
#'
#' @description
#' Provides an overview analysis of weekly email hours.
#' Returns a bar plot showing average weekly email hours by default.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_bar
#' @inherit create_bar return
#'
#' @family Visualization
#' @family Emails
#'
#' @examples
#' # Return a ggplot bar chart
#' email_summary(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' email_summary(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

email_summary <- function(data,
                          hrvar = "Organization",
                          mingroup = 5,
                          return = "plot"){

  create_bar(data = data,
             metric = "Email_hours",
             hrvar = hrvar,
             mingroup = mingroup,
             return = return,
             bar_colour = "darkblue")


}

#' @rdname email_summary
#' @export
email_sum <- email_summary






