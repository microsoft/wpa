# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Plot Internal Network Breadth and Size as a scatter plot
#'
#' @description
#' Plot the internal network metrics for a HR variable as a scatter plot, showing
#' Internal Network Breadth as the vertical axis and Internal Network Size as the
#' horizontal axis.
#'
#' @inheritParams create_bubble
#'
#' @examples
#' ## Return a plot
#' sq_data %>% internal_network_plot(return = "plot")
#'
#' @family Connectivity
#'
#' @export

internal_network_plot <- function(data,
                                  hrvar = "Organization",
                                  mingroup = 5,
                                  return = "plot",
                                  bubble_size = c(1, 8)) {

  data %>%
    rename(`Internal Network Size` = "Internal_network_size",
           `Internal Network Breadth` = "Networking_outside_organization") %>%
    create_bubble(hrvar = hrvar,
                  mingroup = mingroup,
                  metric_x = "Internal Network Size",
                  metric_y = "Internal Network Breadth",
                  return = return,
                  bubble_size = bubble_size)


}
