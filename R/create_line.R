# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Time Trend - Line Chart for any metric
#'
#' @description
#' Provides a week by week view of a selected metric, visualised as line charts.
#' By default returns a line chart for the defined metric,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @details
#' This is a general purpose function that powers all the functions
#' in the package that produce faceted line plots.
#'
#' @template spq-params
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @param ncol Numeric value setting the number of columns on the plot. Defaults
#'   to `NULL` (automatic).
#'
#' @param return String specifying what to return. This must be one of the following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom tidyselect all_of
#'
#' @family Visualization
#' @family Flexible
#' @family Time-series
#'
#' @examples
#' # Return plot of Email Hours
#' sq_data %>% create_line(metric = "Email_hours", return = "plot")
#'
#' # Return plot of Collaboration Hours
#' sq_data %>% create_line(metric = "Collaboration_hours", return = "plot")
#'
#' # Return plot but coerce plot to two columns
#' sq_data %>%
#'   create_line(
#'     metric = "Collaboration_hours",
#'     hrvar = "Organization",
#'     ncol = 2
#'     )
#'
#' # Return plot of Work week span and cut by `LevelDesignation`
#' sq_data %>% create_line(metric = "Workweek_span", hrvar = "LevelDesignation")
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: 'ggplot' object. A faceted line plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @export
create_line <- function(data,
                        metric,
                        hrvar = "Organization",
                        mingroup = 5,
                        ncol = NULL,
                        return = "plot"){

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

  myTable <-
    data %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    select(PersonId, Date, group, all_of(metric)) %>%
    group_by(group) %>%
    mutate(Employee_Count = n_distinct(PersonId)) %>%
    filter(Employee_Count >= mingroup)  # Keep only groups above privacy threshold

  myTable <-
    myTable %>%
    group_by(Date, group) %>%
    summarize(Employee_Count = mean(Employee_Count),
              !!sym(metric) := mean(!!sym(metric)))

  ## Data frame to return
  myTable_return <-
    myTable %>%
    select(Date, group, all_of(metric)) %>%
    spread(Date, !!sym(metric))

  ## Data frame for creating plot
  myTable_plot <-
    myTable %>%
    select(Date, group, all_of(metric)) %>%
    group_by(Date, group) %>%
    summarise_at(vars(all_of(metric)), ~mean(., na.rm = TRUE)) %>%
    ungroup()


  return_plot <- function(){

    myTable_plot %>%
      ggplot(aes(x = Date, y = !!sym(metric))) +
      geom_line(colour = "#1d627e") +
      facet_wrap(.~group, ncol = ncol) +
      scale_fill_gradient(name="Hours", low = "white", high = "red") +
      theme_wpa_basic() +
      theme(strip.background = element_rect(color = "#1d627e",
                                            fill = "#1d627e"),
            strip.text = element_text(size = 10,
                                      colour = "#FFFFFF",
                                      face = "bold")) +
      labs(title = clean_nm,
           subtitle = paste("Total",
                            tolower(clean_nm),
                            "by",
                            tolower(camel_clean(hrvar))),
           x = "Date",
           y = "Weekly hours",
           caption = extract_date_range(data, return = "text")) +
      ylim(0, NA) # Set origin to zero

  }


  if(return == "table"){

    myTable_return

  } else if(return == "plot"){

    return_plot()

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
