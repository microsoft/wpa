# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Heat mapped horizontal bar plot over time for any metric
#'
#' @description
#' Provides a week by week view of a selected Viva Insights metric. By
#' default returns a week by week heatmap bar plot, highlighting the points in
#' time with most activity. Additional options available to return a summary
#' table.
#'
#' @template spq-params
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
#' @param palette Character vector containing colour codes, ranked from the
#' lowest value to the highest value. This is passed directly to
#' `ggplot2::scale_fill_gradientn()`.
#' @param return Character vector specifying what to return, defaults to
#' `"plot"`.
#' Valid inputs are "plot" and "table".
#' @param legend_title String to be used as the title of the legend. Defaults to
#' `"Hours"`.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Visualization
#' @family Flexible
#' @family Time-series
#'
#' @examples
#' create_trend(sq_data, metric = "Collaboration_hours", hrvar = "LevelDesignation")
#'
#' # custom colours
#' create_trend(
#'   sq_data,
#'   metric = "Collaboration_hours",
#'   hrvar = "LevelDesignation",
#'   palette = c(
#'     "#FB6107",
#'     "#F3DE2C",
#'     "#7CB518",
#'     "#5C8001"
#'   )
#'   )
#'
#' @return
#' Returns a 'ggplot' object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

create_trend <- function(data,
                         metric,
                         hrvar = "Organization",
                         mingroup = 5,
                         palette = c("steelblue4",
                                    "aliceblue",
                                    "white",
                                    "mistyrose1",
                                    "tomato1"),
                         return = "plot",
                         legend_title = "Hours"){

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
    select(PersonId, Date, group, !!sym(metric)) %>%
    group_by(group) %>%
    mutate(Employee_Count = n_distinct(PersonId)) %>%
    filter(Employee_Count >= mingroup)  # Keep only groups above privacy threshold

  myTable <-
    myTable %>%
    group_by(Date, group) %>%
    summarize(Employee_Count = mean(Employee_Count, na.rm = TRUE),
              !!sym(metric) := mean(!!sym(metric), na.rm = TRUE))

  myTable_plot <- myTable %>% select(Date, group, !!sym(metric))

  myTable_return <-  myTable_plot %>% tidyr::spread(Date, !!sym(metric))

  plot_object <-
    myTable_plot %>%
    ggplot(aes(x = Date , y = group , fill = !!sym(metric))) +
    geom_tile(height=.5) +
    scale_x_date(position = "top") +
    scale_fill_gradientn(name = legend_title,
                         colours = palette) +
    theme_wpa_basic() +
    theme(axis.line.y = element_blank(), axis.title.y = element_blank()) +
    labs(title = clean_nm,
         subtitle = paste("Hotspots by", tolower(camel_clean(hrvar)))) +
    xlab("Date") +
    ylab(hrvar) +
    labs(caption = extract_date_range(data, return = "text"))

  if(return == "table"){

    myTable_return

  } else if(return == "plot"){

    plot_object

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
