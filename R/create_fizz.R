# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Fizzy Drink / Jitter Scatter Plot for any metric
#'
#' @description
#' Analyzes a selected metric and returns a a 'fizzy' scatter plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' This is a general purpose function that powers all the functions
#' in the package that produce 'fizzy drink' / jitter scatter plots.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector, defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size, defaults to 5.
#' @param return String specifying what to return. This must be one of the following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: ggplot object. A jittered scatter plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Flexible
#'
#' @examples
#' ## Create a fizzy plot for Work Week Span by Level Designation
#' create_fizz(sq_data, metric = "Workweek_span", hrvar = "LevelDesignation", return = "plot")
#'
#' ## Create a summary statistics table for Work Week Span by Organization
#' create_fizz(sq_data, metric = "Workweek_span", hrvar = "Organization", return = "table")
#'
#' ## Create a fizzy plot for Collaboration Hours by Level Designation
#' create_fizz(sq_data, metric = "Collaboration_hours", hrvar = "LevelDesignation", return = "plot")
#' @export

create_fizz <- function(data,
                        metric,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot") {

  ## Check inputs
  required_variables <- c("Date",
                          metric,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Clean metric name
  clean_nm <- us_to_space(metric)

  ## Plot data
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    group_by(PersonId, group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric))) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Get max value
  max_point <- max(plot_data[[metric]]) * 1.2

  plot_legend <-
    plot_data %>%
    group_by(group) %>%
    summarize(Employee_Count = first(Employee_Count)) %>%
    mutate(Employee_Count = paste("n=",Employee_Count))

  plot_object <-
    plot_data %>%
    ggplot(aes(x = group, y = !!sym(metric))) +
    geom_point(size = 1,
               alpha = 1/5,
               color = "#578DB8",
               position = position_jitter(width = 0.1, height = 0.1)) +
    ylim(0, max_point) +
    annotate("text", x = plot_legend$group, y = 0, label = plot_legend$Employee_Count) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    theme_wpa_basic() +
    labs(title = clean_nm,
         subtitle = paste("Distribution of",
                          tolower(clean_nm),
                          "by",
                          camel_clean(hrvar))) +
    xlab(hrvar) +
    ylab(paste("Average", clean_nm)) +
    labs(caption = extract_date_range(data, return = "text"))

  summary_table <-
    plot_data %>%
    select(group, tidyselect::all_of(metric)) %>%
    group_by(group) %>%
    summarise(mean = mean(!!sym(metric)),
              median = median(!!sym(metric)),
              sd = sd(!!sym(metric)),
              min = min(!!sym(metric)),
              max = max(!!sym(metric)),
              range = max - min,
              n = n())

  if(return == "table"){

    summary_table %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
