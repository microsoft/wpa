# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Summary of After-Hours Collaboration Hours
#'
#' @description
#' Provides an overview analysis of after-hours collaboration time.
#' Returns a bar plot showing average weekly after-hours collaboration hours by default.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metric \code{After_hours_collaboration_hours}.
#'
#' @inheritParams create_bar
#' @inherit create_bar return
#'
#' @family Visualization
#' @family After-hours Collaboration
#'
#' @examples
#' # Return a ggplot bar chart
#' afterhours_summary(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' afterhours_summary(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export
afterhours_summary <- function(data,
                              hrvar = "Organization",
                              mingroup = 5,
                              return = "plot"){
  create_bar(data = data,
             metric = "After_hours_collaboration_hours",
             hrvar = hrvar,
             mingroup = mingroup,
             return = return,
             bar_colour = "alert")

}


#' @rdname afterhours_summary
#' @export
afterhours_sum <- afterhours_summary





