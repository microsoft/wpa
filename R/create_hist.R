# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a histogram plot for any metric
#'
#' @description
#' Provides an analysis of the distribution of a selected metric.
#' Returns a faceted histogram by default.
#' Additional options available to return the underlying frequency table.
#'
#' @template spq-params
#' @param metric String containing the name of the metric,
#' e.g. "Collaboration_hours"
#'
#' @param binwidth Numeric value for setting `binwidth` argument within
#'   `ggplot2::geom_histogram()`. Defaults to 1.
#'
#' @param ncol Numeric value setting the number of columns on the plot. Defaults
#'   to `NULL` (automatic).
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'   - `"data"`
#'   - `"frequency"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object. A faceted histogram for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'   - `"data"`: data frame. Data with calculated person averages.
#'   - `"frequency`: list of data frames. Each data frame contains the
#'   frequencies used in each panel of the plotted histogram.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom tidyr spread
#' @importFrom stats median
#' @importFrom stats sd
#'
#' @family Flexible
#'
#' @examples
#' # Return plot for whole organization
#' create_hist(sq_data, metric = "Collaboration_hours", hrvar = NULL)
#'
#' # Return plot
#' create_hist(sq_data, metric = "Collaboration_hours", hrvar = "Organization")
#'
#' # Return plot but coerce plot to two columns
#' create_hist(sq_data, metric = "Collaboration_hours", hrvar = "Organization", ncol = 2)
#'
#' # Return summary table
#' create_hist(sq_data,
#'             metric = "Collaboration_hours",
#'             hrvar = "Organization",
#'             return = "table")
#' @export

create_hist <- function(data,
                        metric,
                        hrvar = "Organization",
                        mingroup = 5,
                        binwidth = 1,
						ncol = NULL,
                        return = "plot") {

  ## Check inputs
  required_variables <- c("Date",
                          metric,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Clean metric name
  clean_nm <- us_to_space(metric)

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Basic Data for bar plot
  ## Calculate person-averages
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric))) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Employee count / base size table
  plot_legend <-
    plot_data %>%
    group_by(group) %>%
    summarize(Employee_Count = first(Employee_Count)) %>%
    mutate(Employee_Count = paste("n=",Employee_Count))

  ## Bar plot

  plot_object <-
    plot_data %>%
    ggplot(aes(x = !!sym(metric))) +
    geom_histogram(binwidth = binwidth, colour = "white", fill="#34b1e2") +
    facet_wrap(group ~ ., ncol = ncol) +
    theme_wpa_basic() +
    theme(strip.background = element_rect(color = "#1d627e",
                                          fill = "#1d627e"),
          strip.text = element_text(size = 10,
                                    colour = "#FFFFFF",
                                    face = "bold")) +
    labs(title = clean_nm,
         subtitle = paste("Distribution of", tolower(clean_nm), "by", tolower(camel_clean(hrvar)))) +
    xlab(clean_nm) +
    ylab("Number of employees") +
    labs(caption = extract_date_range(data, return = "text"))

  ## Table to return
  return_table <-
    plot_data %>%
    group_by(group) %>%
    summarise(
      mean = mean(!!sym(metric), na.rm = TRUE),
      median = median(!!sym(metric), na.rm = TRUE),
      max = max(!!sym(metric), na.rm = TRUE),
      min = min(!!sym(metric), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group")


  if(return == "table"){

    return_table

  } else if(return == "plot"){

    return(plot_object)

  } else if(return == "frequency"){

    ggplot2::ggplot_build(plot_object)$data[[1]] %>%
      select(group,
             PANEL,
             x,
             xmin,
             xmax,
             y) %>%
      group_split(PANEL)

  } else if(return == "data"){

    plot_data

  } else {

    stop("Please enter a valid input for `return`.")

  }
}
