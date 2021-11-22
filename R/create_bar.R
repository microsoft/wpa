# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Mean Bar Plot for any metric
#'
#' @description
#' Provides an overview analysis of a selected metric by calculating a mean per
#' metric.
#' Returns a bar plot showing the average of a selected metric by default.
#' Additional options available to return a summary table.
#'
#' @template spq-params
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @param bar_colour String to specify colour to use for bars.
#' In-built accepted values include `"default"` (default), `"alert"` (red), and
#' `"darkblue"`. Otherwise, hex codes are also accepted. You can also supply
#' RGB values via `rgb2hex()`.
#' @param na.rm A logical value indicating whether `NA` should be stripped
#' before the computation proceeds. Defaults to `FALSE`.
#' @param percent Logical value to determine whether to show labels as
#'   percentage signs. Defaults to `FALSE`.
#' @param plot_title An option to override plot title.
#' @param plot_subtitle An option to override plot subtitle.
#' @param legend_lab String. Option to override legend title/label. Defaults to
#' `NULL`, where the metric name will be populated instead.
#' @param rank String specifying how to rank the bars. Valid inputs are:
#'   - `"descending"` - ranked highest to lowest from top to bottom (default).
#'   - `"ascending"` - ranked lowest to highest from top to bottom.
#'   - `NULL` - uses the original levels of the HR attribute.
#' @param xlim An option to set max value in x axis.
#' @param text_just `r lifecycle::badge('experimental')` A numeric value
#'   controlling for the horizontal position of the text labels. Defaults to
#'   0.5.
#' @param text_colour `r lifecycle::badge('experimental')` String to specify
#'   colour to use for the text labels. Defaults to `"#FFFFFF"`.
#'
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: 'ggplot' object. A bar plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @importFrom scales wrap_format
#' @importFrom stats reorder
#'
#' @family Visualization
#' @family Flexible
#'
#' @examples
#' # Return a ggplot bar chart
#' create_bar(sq_data, metric = "Collaboration_hours", hrvar = "LevelDesignation")
#'
#' # Change bar colour
#' create_bar(sq_data,
#'            metric = "After_hours_collaboration_hours",
#'            bar_colour = "alert")
#'
#' # Custom data label positions and formatting
#' sq_data %>%
#'   create_bar(
#'     metric = "Meetings",
#'     text_just = 1.1,
#'     text_colour = "black",
#'     xlim = 20)
#'
#' # Return a summary table
#' create_bar(sq_data,
#'            metric = "Collaboration_hours",
#'            hrvar = "LevelDesignation",
#'            return = "table")
#' @export
create_bar <- function(data,
                       metric,
                       hrvar = "Organization",
                       mingroup = 5,
                       return = "plot",
                       bar_colour = "default",
                       na.rm = FALSE,
                       percent = FALSE,
                       plot_title = us_to_space(metric),
                       plot_subtitle = paste("Average by", tolower(camel_clean(hrvar))),
                       legend_lab = NULL,
                       rank = "descending",
                       xlim = NULL,
                       text_just = 0.5,
                       text_colour = "#FFFFFF"){

  ## Check inputs
  required_variables <- c("Date",
                          metric,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Handle `legend_lab`
  if(is.null(legend_lab)){
    legend_lab <- gsub("_", " ", metric)
  }

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Clean metric name
  clean_nm <- us_to_space(metric)

  ## Data for bar plot
  plot_data <-
    data %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric), na.rm = na.rm)) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Colour bar override
  if(bar_colour == "default"){

    bar_colour <- "#34b1e2"

  } else if(bar_colour == "alert"){

    bar_colour <- "#FE7F4F"

  } else if(bar_colour == "darkblue"){

    bar_colour <- "#1d627e"

  }

  ## Bar plot
  plot_object <- data %>%
    create_stacked(
      metrics = metric,
      hrvar = hrvar,
      mingroup = mingroup,
      stack_colours = bar_colour,
      percent = percent,
      plot_title = plot_title,
      plot_subtitle = plot_subtitle,
      legend_lab = legend_lab,
      return = "plot",
      rank = rank,
      xlim = xlim,
      text_just = text_just,
      text_colour = text_colour
      )

  summary_table <-
    plot_data %>%
    select(group, !!sym(metric)) %>%
    group_by(group) %>%
    summarise(!!sym(metric) := mean(!!sym(metric)),
              n = n())

  if(return == "table"){

    return(summary_table)

    } else if(return == "plot"){

    return(plot_object)

    } else {

    stop("Please enter a valid input for `return`.")

    }
}
