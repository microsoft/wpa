# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Distribution of Manager 1:1 Time as a 100% stacked bar
#'
#' @description
#' Analyze Manager 1:1 Time distribution.
#' Returns a stacked bar plot of different buckets of 1:1 time.
#' Additional options available to return a table with distribution elements.
#'
#' @inheritParams create_dist
#' @inherit create_dist return
#'
#' @family Visualization
#' @family Managerial Relations
#'
#' @examples
#' # Return plot
#' one2one_dist(sq_data, hrvar = "Organization", return = "plot")
#'
#' # Return summary table
#' one2one_dist(sq_data, hrvar = "Organization", return = "table")
#' @export

one2one_dist <- function(data,
                         hrvar = "Organization",
                         mingroup = 5,
                         dist_colours = c("#facebc",
                                          "#fcf0eb",
                                          "#b4d5dd",
                                          "#bfe5ee"),
                         return = "plot",
                         cut = c(5, 15, 30)) {

  cleaned_data <-
    data %>%
    mutate(`Scheduled 1:1 meeting minutes with manager` = Meeting_hours_with_manager_1_on_1 * 60)

  create_dist(data = cleaned_data,
              metric = "Scheduled 1:1 meeting minutes with manager",
              hrvar = hrvar,
              mingroup = mingroup,
              dist_colours = dist_colours,
              return = return,
              cut = cut,
              unit = "minutes")

}
