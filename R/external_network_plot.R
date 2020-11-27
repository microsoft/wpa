# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Plot External Network Breadth and Size as a scatter plot
#'
#' @description
#' Plot the external network metrics for a HR variable as a scatter plot, showing
#' External Network Breadth as the vertical axis and External Network Size as the
#' horizontal axis.
#'
#' @inheritParams create_bubble
#'
#' @examples
#' ## Return a plot
#' sq_data %>% external_network_plot(return = "plot")
#'
#' @family Connectivity
#'
#' @export

external_network_plot <- function(data,
                                  hrvar = "Organization",
                                  mingroup = 5,
                                  return = "plot",
                                  bubble_size = c(1, 8)){

  data %>%
    rename(`External Network Size` = "External_network_size",
           `External Network Breadth` = "Networking_outside_company") %>%
    create_bubble(hrvar = hrvar,
                  mingroup = mingroup,
                  metric_x = "External Network Size",
                  metric_y = "External Network Breadth",
                  return = return,
                  bubble_size = bubble_size)

}
