# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Run a summary of Key Metrics without aggregation
#'
#' @description
#' Return a heatmapped table directly from the aggregated / summarised data.
#' Unlike `keymetrics_scan()` which performs a person-level aggregation, there
#' is no calculation for `keymetrics_scan_asis()` and the values are rendered as
#' they are passed into the function.
#'
#' @param data data frame containing data to plot. It is recommended to provide
#'   data in a 'long' table format where one grouping column forms the rows, a
#'   second column forms the columns, and a third numeric columns forms the
#' @param row_var String containing name of the grouping variable that will form
#'   the rows of the heatmapped table.
#' @param col_var String containing name of the grouping variable that will form
#'   the columns of the heatmapped table.
#' @param group_var String containing name of the grouping variable by which
#' heatmapping would apply. Defaults to `col_var`.
#' @param value_var String containing name of the value variable that will form
#' the values of the heatmapped table. Defaults to `"value"`.
#' @param title Title of the plot.
#' @param subtitle Subtitle of the plot.
#' @param caption Caption of the plot.
#' @param ylab Y-axis label for the plot (group axis)
#' @param xlab X-axis label of the plot (bar axis).
#' @param rounding Numeric value to specify number of digits to show in data
#'   labels
#' @param low String specifying colour code to use for low-value metrics.
#'   Arguments are passed directly to `ggplot2::scale_fill_gradient2()`.
#' @param mid String specifying colour code to use for mid-value metrics.
#'   Arguments are passed directly to `ggplot2::scale_fill_gradient2()`.
#' @param high String specifying colour code to use for high-value metrics.
#'   Arguments are passed directly to `ggplot2::scale_fill_gradient2()`.
#' @param textsize A numeric value specifying the text size to show in the plot.
#'
#' @return
#' ggplot object for a heatmap table.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # Compute summary table
#' out_df <-
#'   sq_data %>%
#'   group_by(Organization) %>%
#'   summarise(
#'     across(
#'       .cols = c(
#'         Workweek_span,
#'         Collaboration_hours
#'         ),
#'       .fns = ~median(., na.rm = TRUE)
#'       ),
#'       .groups = "drop"
#'     ) %>%
#' tidyr::pivot_longer(
#'   cols = c("Workweek_span", "Collaboration_hours"),
#'   names_to = "metrics"
#' )
#'
#' keymetrics_scan_asis(
#'   data = out_df,
#'   col_var = "metrics",
#'   row_var = "Organization"
#' )
#'
#' # Show data the other way round
#' keymetrics_scan_asis(
#'   data = out_df,
#'   col_var = "Organization",
#'   row_var = "metrics",
#'   group_var = "metrics"
#' )
#'
#' @export
#'
keymetrics_scan_asis <- function(
  data,
  row_var,
  col_var,
  group_var = col_var,
  value_var = "value",
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  ylab = row_var,
  xlab = "Metrics",
  rounding = 1,
  low = rgb2hex(7, 111, 161),
  mid = rgb2hex(241, 204, 158),
  high = rgb2hex(216, 24, 42),
  textsize = 2
){

  # Transform to long data format
  myTable_long <-
    data %>%
    mutate(!!sym(group_var) := factor(!!sym(group_var))) %>%
    group_by(!!sym(group_var)) %>%
    # Heatmap by row
    mutate(value_rescaled = maxmin(!!sym(value_var))) %>%
    ungroup()

  plot_object <-
    myTable_long %>%
    ggplot(aes(x = !!sym(row_var),
               y = stats::reorder(!!sym(col_var), desc(!!sym(col_var))))) +
    geom_tile(aes(fill = value_rescaled),
              colour = "#FFFFFF",
              size = 2) +
    geom_text(aes(label = round(value, rounding)), size = textsize) +
    # Fill is contingent on max-min scaling
    scale_fill_gradient2(
      low = low,
      mid = mid,
      high = high,
      midpoint = 0.5,
      breaks = c(0, 0.5, 1),
      labels = c("Minimum", "", "Maximum"),
      limits = c(0, 1)
      ) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(labels = us_to_space) +
    theme_wpa_basic() +
    theme(axis.line = element_line(color = "#FFFFFF")) +
    labs(title = title,
         subtitle = subtitle,
         y = " ",
         x = " ",
         fill = " ",
         caption = caption) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 0),
      plot.title = element_text(color="grey40", face="bold", size=20)
      )

  plot_object

}
