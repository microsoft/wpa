# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a line chart that tracks metrics over time with a 4-week
#'   rolling average
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Create a two-series line chart that visualizes a set of metric over time for
#' the selected population, with one of the series being a four-week rolling
#' average.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#'   percentage signs. Defaults to `FALSE`.
#' @param plot_title An option to override plot title.
#' @param plot_subtitle An option to override plot subtitle.
#' @param percent Logical value to determine whether to show labels as
#'   percentage signs. Defaults to `FALSE`.
#'
#' @examples
#' sq_data %>%
#'   create_tracking(
#'     metric = "Collaboration_hours",
#'     percent = FALSE
#'   )
#'
#' @family Visualization
#' @family Flexible
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#'
#' @export

create_tracking <- function(data,
                            metric,
                            plot_title = us_to_space(metric),
                            plot_subtitle = "Measure over time",
                            percent = FALSE){

  data$Date <- as.Date(data$Date, "%m/%d/%Y")

  min_date <- data %>% extract_date_range() %>% pull(Start)
  max_date <- data %>% extract_date_range() %>% pull(End)

  # Set variables
  metrics <- NULL
  `4 week rolling average` <- NULL
  `Weekly average` <- NULL

  data %>%
    group_by(Date) %>%
    summarise(across(.cols = metric,
                     .fns = ~mean(., na.rm = TRUE)),
              .groups = "drop") %>%
    mutate(
      lag0 = lag(!!sym(metric), 0),
      lag1 = lag(!!sym(metric), 1),
      lag2 = lag(!!sym(metric), 2),
      lag3 = lag(!!sym(metric), 3)
    ) %>%
    mutate(`4 week rolling average` = select(., paste0("lag", 0:3)) %>%
             apply(1, function(x) mean(x, na.rm = TRUE))) %>% # Use all available data
    select(-paste0("lag", 0:3)) %>%
    rename(`Weekly average` = metric) %>%
    pivot_longer(cols = c(`Weekly average`, `4 week rolling average`),
                 names_to = "metrics",
                 values_to = "value") %>%
    drop_na(value) %>%
    ggplot(aes(x = Date,
               y = value,
               colour = metrics)) +
    geom_line(size = 1) +
    scale_colour_manual(
      values = c(
        "Weekly average" = rgb2hex(67, 189, 211),
        "4 week rolling average" = rgb2hex(0, 82, 101)),
      labels = us_to_space,
      guide = guide_legend(reverse = TRUE)
    ) +
    { if(percent == FALSE){
      scale_y_continuous(
        limits = c(0, NA)
      )
    } else if(percent == TRUE){
      scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent
      )
    }} +
    scale_x_date(position = "top",
                 limits = c(min_date, max_date),
                 date_breaks = "2 weeks") +
    theme_wpa_basic() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_line(color="gray"),
          panel.grid.major.y = element_line(colour = "#D9E7F7", size = 5)) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      caption = extract_date_range(data, return = "text")
    )
}
