# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a bar chart without aggregation for any metric
#'
#' @description
#' This function creates a bar chart directly from the aggregated / summarised
#' data. Unlike `create_bar()` which performs a person-level aggregation, there
#' is no calculation for `create_bar_asis()` and the values are rendered as they
#' are passed into the function.
#'
#' @param data Plotting data as a data frame.
#' @param group_var String containing name of variable for the group.
#' @param bar_var String containing name of variable representing the value of
#'   the bars.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param ylab Y-axis label for the plot (group axis)
#' @param xlab X-axis label of the plot (bar axis).
#' @param percent Logical value to determine whether to show labels as
#'   percentage signs. Defaults to FALSE.
#' @param bar_colour String to specify colour to use for bars.
#' In-built accepted values include "default" (default), "alert" (red), and
#' "darkblue". Otherwise, hex codes are also accepted. You can also supply
#' RGB values via `rgb2hex()`.
#' @param rounding Numeric value to specify number of digits to show in data
#'   labels
#'
#' @return
#' ggplot object. A horizontal bar plot.
#'
#' @examples
#' # Creating a custom bar plot without mean aggregation
#' library(dplyr)
#'
#' sq_data %>%
#'   group_by(Organization) %>%
#'   summarise(across(.cols = Meeting_hours,
#'                    .fns = ~sum(., na.rm = TRUE))) %>%
#'   create_bar_asis(group_var = "Organization",
#'                   bar_var = "Meeting_hours",
#'                   title = "Total Meeting Hours over period",
#'                   subtitle = "By Organization",
#'                   caption = extract_date_range(sq_data, return = "text"),
#'                   bar_colour = "darkblue",
#'                   rounding = 0)
#'
#' @import ggplot2
#' @import dplyr
#'
#' @family General
#'
#' @export
create_bar_asis <- function(data,
                            group_var,
                            bar_var,
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            ylab = group_var,
                            xlab = bar_var,
                            percent = FALSE,
                            bar_colour = "default",
                            rounding = 1){

  ## Colour bar override
  if(bar_colour == "default"){

    bar_colour <- "#34b1e2"

  } else if(bar_colour == "alert"){

    bar_colour <- "#FE7F4F"

  } else if(bar_colour == "darkblue"){

    bar_colour <- "#1d627e"

  }

  up_break <- max(data[[bar_var]], na.rm = TRUE) * 1.3

  if(percent == FALSE){
    returnPlot <-
      data %>%
      ggplot(aes(x = reorder(!!sym(group_var), !!sym(bar_var)), y = !!sym(bar_var))) +
      geom_col(fill = bar_colour) +
      geom_text(aes(label = round(!!sym(bar_var), digits = rounding)),
                hjust = -0.25,
                color = "#000000",
                fontface = "bold",
                size = 4)
  } else if(percent == TRUE){
    returnPlot <-
      data %>%
      ggplot(aes(x = reorder(!!sym(group_var), !!sym(bar_var)), y = !!sym(bar_var))) +
      geom_col(fill = bar_colour) +
      geom_text(aes(label = scales::percent(!!sym(bar_var),
                                            accuracy = 10 ^ -rounding)),
                hjust = -0.25,
                color = "#000000",
                fontface = "bold",
                size = 4)

  }

  returnPlot +
    scale_y_continuous(limits = c(0, up_break)) +
    coord_flip() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         y = xlab,
         x = ylab) +
    theme_wpa_basic()
}
