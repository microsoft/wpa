# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Period comparison scatter plot for any two metrics
#'
#' @description
#' Returns two side-by-side scatter plots representing two selected metrics,
#' using colour to map an HR attribute and size to represent number of employees.
#' Returns a faceted scatter plot by default, with additional options
#' to return a summary table.
#'
#' @details
#' This is a general purpose function that powers all the functions
#' in the package that produce faceted scatter plots.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector,
#' defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param metric_x Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param metric_y Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param before_start Start date of "before" time period in YYYY-MM-DD
#' @param before_end End date of "before" time period in YYYY-MM-DD
#' @param after_start Start date of "after" time period in YYYY-MM-DD
#' @param after_end End date of "after" time period in YYYY-MM-DD
#' @param before_label String to specify a label for the "before" period. Defaults to "Period 1".
#' @param after_label String to specify a label for the "after" period. Defaults to "Period 2".
#' @param mingroup Numeric value setting the privacy threshold / minimum group size.
#' Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#'
#' @family Visualization
#' @family Flexible
#' @family Time-series
#'
#' @return
#' Returns a 'ggplot' object showing two scatter plots side by side representing
#' the two periods.
#'
#' @examples
#' # Return plot
#' create_period_scatter(sq_data,
#'                       hrvar = "LevelDesignation",
#'                       before_start = "2019-11-03",
#'                       before_end = "2019-12-31",
#'                       after_start = "2020-01-01",
#'                       after_end = "2020-01-26")
#'
#' # Return a summary table
#' create_period_scatter(sq_data, before_end = "2019-12-31", return = "table")
#'
#'
#' @export
create_period_scatter <- function(data,
                                  hrvar = "Organization",
                                  metric_x = "Multitasking_meeting_hours",
                                  metric_y = "Meeting_hours",
                                  before_start = min(as.Date(data$Date, "%m/%d/%Y")),
                                  before_end,
                                  after_start = as.Date(before_end) + 1,
                                  after_end = max(as.Date(data$Date, "%m/%d/%Y")),
                                  before_label = "Period 1",
                                  after_label = "Period 2",
                                  mingroup = 5,
                                  return = "plot"){

  ## Check inputs
  ## Update these column names as per appropriate
  required_variables <- c("Date",
                          hrvar,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)


  daterange_1_start <- as.Date(before_start)
  daterange_1_end <- as.Date(before_end)
  daterange_2_start <- as.Date(after_start)
  daterange_2_end <- as.Date(after_end)

  # Fix dates format for WpA Queries
  WpA_dataset <- data %>% mutate(Date = as.Date(Date, "%m/%d/%Y"))

  # Check for dates in data file
  if (daterange_1_start < min(WpA_dataset$Date) |
      daterange_1_start > max(WpA_dataset$Date) |
      daterange_1_end < min(WpA_dataset$Date) |
      daterange_1_end > max(WpA_dataset$Date) |
      daterange_2_start < min(WpA_dataset$Date) |
      daterange_2_start > max(WpA_dataset$Date) |
      daterange_2_end < min(WpA_dataset$Date) |
      daterange_2_end > max(WpA_dataset$Date)) {
    stop('Dates not found in dataset')
    geterrmessage()
  }

  ## Employee count
  emp_count <-
    WpA_dataset %>%
    group_by(!!sym(hrvar)) %>%
    summarise(n = n_distinct(PersonId))

  data_p1 <-
    WpA_dataset %>%
    rename(group = hrvar) %>%
    filter(between(Date, daterange_1_start, daterange_1_end)) %>%
    group_by(PersonId, group) %>%
    summarise_at(vars(!!sym(metric_x), !!sym(metric_y)), ~mean(.)) %>%
    ungroup() %>%
    group_by(group) %>%
    summarise_at(vars(!!sym(metric_x), !!sym(metric_y)), ~mean(., na.rm = TRUE)) %>%
    mutate(Period = before_label) %>%
    left_join(emp_count, by = c(group = hrvar)) %>%
    filter(n >= mingroup)

  data_p2 <-
    WpA_dataset %>%
    rename(group = hrvar) %>%
    filter(between(Date, daterange_2_start, daterange_2_end)) %>%
    group_by(PersonId, group) %>%
    summarise_at(vars(!!sym(metric_x), !!sym(metric_y)), ~mean(.)) %>%
    ungroup() %>%
    group_by(group) %>%
    summarise_at(vars(!!sym(metric_x), !!sym(metric_y)), ~mean(., na.rm = TRUE)) %>%
    mutate(Period = after_label) %>%
    left_join(emp_count, by = c(group = hrvar)) %>%
    filter(n >= mingroup)

  ## bind data
  data_both <- rbind(data_p1, data_p2)

  date_range_str <-
    paste("Data from",
          daterange_1_start,
          "to",
          daterange_1_end,
          "and",
          daterange_2_start,
          "to",
          daterange_2_end)

  clean_x <- us_to_space(metric_x)
  clean_y <- us_to_space(metric_y)

  plot_title <-
    paste(clean_x, "and", clean_y)


  plot_object <-
    data_both %>%
    ggplot(aes(x = !!sym(metric_x),
               y = !!sym(metric_y),
               colour = group,
               size = n)) +
    geom_point(alpha = 0.5) +
    scale_size(range = c(1, 20)) +
    facet_wrap(.~Period) +
    guides(size = FALSE) +
	theme_wpa_basic() +
    theme(legend.position = "bottom", 
	strip.background = element_rect(color = "#1d627e",
                                          fill = "#1d627e"),
          strip.text = element_text(size = 10,
                                    colour = "#FFFFFF",
                                    face = "bold")) +
    ggtitle(plot_title,
            subtitle = paste("Comparison of weekly averages by ", tolower(camel_clean(hrvar)))) +
    ylab(clean_y) +
    xlab(clean_x) +
    labs(caption = date_range_str) 
	
	
  if(return == "table"){

    # return(myTable_return)
    return(data_both)

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}


