# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Box Plot for any metric
#'
#' @description
#' Analyzes a selected metric and returns a box plot by default.
#' Additional options available to return a table with distribution elements.
#'
#' @details
#' This is a general purpose function that powers all the functions
#' in the package that produce box plots.
#'
#' @template spq-params
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: 'ggplot' object. A box plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
#' # Create a fizzy plot for Work Week Span by Level Designation
#' create_boxplot(sq_data,
#'                metric = "Workweek_span",
#'                hrvar = "LevelDesignation",
#'                return = "plot")
#'
#' # Create a summary statistics table for Work Week Span by Organization
#' create_boxplot(sq_data,
#'                metric = "Workweek_span",
#'                hrvar = "Organization",
#'                return = "table")
#'
#' # Create a fizzy plot for Collaboration Hours by Level Designation
#' create_boxplot(sq_data,
#'                metric = "Collaboration_hours",
#'                hrvar = "LevelDesignation",
#'                return = "plot")
#' @export

create_boxplot <- function(data,
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

  ## summary table
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

  ## group order
  group_ord <-
    summary_table %>%
    arrange(desc(mean)) %>%
    pull(group)

  plot_object <-
    plot_data %>%
    mutate(group = factor(group, levels = group_ord)) %>%
    ggplot(aes(x = group, y = !!sym(metric))) +
    geom_boxplot(color = "#578DB8") +
    ylim(0, max_point) +
    annotate("text", x = plot_legend$group, y = 0, label = plot_legend$Employee_Count) +
    scale_x_discrete(labels = scales::wrap_format(10)) +
    theme_wpa_basic() +
    theme(axis.text=element_text(size=12),
          axis.text.x = element_text(angle = 30, hjust = 1),
          plot.title = element_text(color="grey40", face="bold", size=18),
          plot.subtitle = element_text(size=14),
          legend.position = "top",
          legend.justification = "right",
          legend.title=element_text(size=14),
          legend.text=element_text(size=14)) +
    labs(title = clean_nm,
         subtitle = paste("Distribution of",
                          tolower(clean_nm),
                          "by",
                          tolower(camel_clean(hrvar)))) +
    xlab(hrvar) +
    ylab(paste("Average", clean_nm)) +
    labs(caption = extract_date_range(data, return = "text"))



  if(return == "table"){

    summary_table %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else if(return == "data"){

    plot_data %>%
      mutate(group = factor(group, levels = group_ord)) %>%
      arrange(desc(group))

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
