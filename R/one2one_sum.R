# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Manager 1:1 Time Summary
#'
#' @description
#' Provides an overview analysis of Manager 1:1 Time. Returns a bar plot showing
#' average weekly minutes of Manager 1:1 Time by default. Additional options
#' available to return a summary table.
#'
#' @inheritParams create_bar
#' @inherit create_bar return
#'
#' @family Visualization
#' @family Managerial Relations
#'
#' @examples
#' # Return a ggplot bar chart
#' one2one_sum(sq_data, hrvar = "LevelDesignation")
#'
#' # Return a summary table
#' one2one_sum(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

one2one_sum <- function(data,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot"){

  cleaned_data <-
    data %>%
    mutate(`Scheduled 1:1 meeting minutes with manager` = Meeting_hours_with_manager_1_on_1 * 60)


  create_bar(data = cleaned_data,
             hrvar = hrvar,
             mingroup = mingroup,
             metric = "Scheduled 1:1 meeting minutes with manager",
             return = return,
             bar_colour = "darkblue")

}

#' @rdname one2one_sum
#' @export
one2one_summary <- one2one_sum








