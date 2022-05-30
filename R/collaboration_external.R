# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration -  External / Internal
#'
#' @description
#' Provides a split of 'Weekly Digital Collaboration' between internal and external accounts.
#' Returns a stacked bar plot of internal and external collaboration.
#' Additional options available to return a summary table.
#'
#' @inheritParams create_stacked
#' @inherit create_stacked return
#'
#' @family Visualization
#' @family Collaboration
#'
#' @examples
#' # Return a  plot
#' collaboration_external(sq_data, hrvar = "LevelDesignation")
#'
#' # Return summary table
#' collaboration_external(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @export

collaboration_external <- function(data,
                                hrvar = "Organization",
                                mingroup = 5,
                                return = "plot"){


# Calculate Internal / External Collaboration time 
plot_data <-  data %>% mutate(Internal_collaboration_hours =  Collaboration_hours - Collaboration_hours_external)   %>%  mutate(External_collaboration_hours =  Collaboration_hours_external) 
  
# Plot Internal / External Collaboration time by Organization
plot_data %>%  create_stacked(hrvar = hrvar, metrics = c("Internal_collaboration_hours", "External_collaboration_hours"),   plot_title = "Collaboration Hours (Internal / External)",  stack_colours = c("#1d7e6a","#1d327e"), mingroup = mingroup, return = return)

}
