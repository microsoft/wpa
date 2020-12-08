# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a bubble plot with two selected Workplace Analytics metrics (General Purpose), with size
#' representing the number of employees in the group.
#'
#' @description
#' Returns a bubble plot of two selected metrics, using size to map the number of employees.
#'
#' @details
#' This is a general purpose function that powers all the functions
#' in the package that produce bubble plots.
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
#' @param bubble_size A numeric vector of length two to specify the size range of the bubbles
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#'
#' @family General
#'
#' @examples
#' create_bubble(sq_data,
#' "Internal_network_size",
#' "External_network_size",
#' "Organization")
#'
#' create_bubble(sq_data,
#' "Generated_workload_call_hours",
#' "Generated_workload_email_hours",
#' "Organization", mingroup = 100, return = "plot")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
create_bubble <- function(data,
                          metric_x,
                          metric_y,
                          hrvar = "Organization",
                          mingroup = 5,
                          return = "plot",
                          bubble_size = c(1, 10)){

  ## Check inputs
  required_variables <- c(hrvar,
                          metric_x,
                          metric_y,
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

  ## Clean metric names
  clean_x <- gsub(pattern = "_", replacement = " ", x = metric_x)
  clean_y <- gsub(pattern = "_", replacement = " ", x = metric_y)

  myTable <-
    data %>%
    group_by(PersonId, !!sym(hrvar)) %>%
    summarise_at(vars(!!sym(metric_x), !!sym(metric_y)), ~mean(., na.rm = TRUE)) %>%
    group_by(!!sym(hrvar)) %>%
    summarise_at(vars(!!sym(metric_x), !!sym(metric_y)), ~mean(., na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(hrvar_count(data, hrvar = hrvar, return = "table"),
              by = hrvar) %>%
    filter(n >= mingroup)

  plot_object <-
    myTable %>%
    ggplot(aes(x = !!sym(metric_x),
               y = !!sym(metric_y),
               label = !!sym(hrvar))) +
    geom_point(alpha = 0.5, color = rgb2hex(0, 120, 212), aes(size = n)) +
    ggrepel::geom_text_repel(size = 3) +
    labs(title = paste0(clean_x, " and ", clean_y),
         subtitle = paste("By", camel_clean(hrvar)),
         caption = paste("Total employees =", sum(myTable$n), "|",
                         extract_date_range(data, return = "text"))) +
    xlab(clean_x) +
    ylab(clean_y) +
    scale_size(range = bubble_size) +
    theme_wpa_basic()

  if(return == "table"){

    return(myTable)

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}

