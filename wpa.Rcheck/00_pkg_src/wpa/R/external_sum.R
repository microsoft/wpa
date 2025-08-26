# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title External Collaboration Summary
#'
#' @description
#' Provides an overview analysis of 'External Collaboration'.
#' Returns a stacked bar plot of internal and external collaboration.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_stacked
#' @inherit create_stacked return
#'
#' @family Visualization
#' @family External Collaboration
#'
#' @examples
#' # Return a plot
#' external_sum(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' external_sum(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

external_sum <- function(data,
								hrvar = "Organization",
                                mingroup = 5,
								stack_colours = c("#1d327e", "#1d7e6a"),
                                return = "plot"){


# Calculate Internal / External Collaboration time
plot_data <-  data %>% mutate(Internal_hours=  Collaboration_hours - Collaboration_hours_external)   %>%  mutate(External_hours=  Collaboration_hours_external)

# Plot Internal / External Collaboration time by Organization
plot_data %>%  create_stacked(hrvar = hrvar, metrics = c("Internal_hours", "External_hours"),   plot_title = "Internal and External Collaboration Hours",  stack_colours = stack_colours, mingroup = mingroup, return = return)

}


#' @rdname external_sum
#' @export
external_summary <- external_sum


