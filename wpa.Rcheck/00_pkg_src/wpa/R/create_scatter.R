# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Create a Scatter plot with two selected Viva Insights metrics (General Purpose)
#'
#' @description
#' Returns a scatter plot of two selected metrics, using colour to map
#' an HR attribute.
#' Returns a scatter plot by default, with additional options
#' to return a summary table.
#'
#' @details
#' This is a general purpose function that powers all the functions
#' in the package that produce scatter plots.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric_x Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param metric_y Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#'  but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
#' create_scatter(sq_data,
#' "Internal_network_size",
#' "External_network_size",
#' "Organization")
#'
#' create_scatter(sq_data,
#' "Generated_workload_call_hours",
#' "Generated_workload_email_hours",
#' "Organization", mingroup = 100, return = "plot")
#'
#' @return
#' Returns a 'ggplot' object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
create_scatter <- function(data,
                           metric_x,
                           metric_y,
                           hrvar = "Organization",
                           mingroup = 5,
                           return = "plot"){

  ## Check inputs
  required_variables <- c(hrvar,
                          metric_x,
                          metric_y,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Extract values violating privacy threshold
  violate_thres_chr <-
    data %>%
    group_by(!!sym(hrvar)) %>%
    summarise(n = n_distinct(PersonId)) %>%
    filter(n < mingroup) %>%
    pull(!!sym(hrvar))

  ## Clean metric names
  clean_x <- us_to_space(metric_x)
  clean_y <- us_to_space(metric_y)

  myTable <-
    data %>%
    filter(!(!!sym(hrvar) %in% violate_thres_chr)) %>%
    group_by(PersonId, !!sym(hrvar)) %>%
    summarise_at(vars(!!sym(metric_x),
                      !!sym(metric_y)),
                 ~mean(.)) %>%
    ungroup()

  plot_object <-
    myTable %>%
    ggplot(aes(x = !!sym(metric_x),
               y = !!sym(metric_y),
               colour = !!sym(hrvar))) +
    geom_point(alpha = 0.5) +
    labs(title = paste0(clean_x, " and ", clean_y),
	subtitle = paste("Distribution of employees by", tolower(camel_clean(hrvar))),
         caption = extract_date_range(data, return = "text")) +
    xlab(clean_x) +
    ylab(clean_y) +
    theme_wpa_basic()

  myTable_return <-
    myTable %>%
    group_by(!!sym(hrvar)) %>%
    summarise_at(vars(!!sym(metric_x),
                      !!sym(metric_y)),
                 ~mean(.))

  if(return == "table"){

    return(myTable_return)

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}

