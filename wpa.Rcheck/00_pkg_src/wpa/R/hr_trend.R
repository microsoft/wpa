# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Employee count over time
#'
#' @description Returns a line chart showing the change in
#' employee count over time. Part of a data validation process to check
#' for unusual license growth / declines over time.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object. A line plot showing employee count over time.
#'   - `"table"`: data frame containing a summary table.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' # Return plot
#' hr_trend(dv_data)
#'
#' # Return summary table
#' hr_trend(dv_data, return = "table")
#'
#' @family Visualization
#' @family Data Validation
#'
#' @export
hr_trend <- function(data, return = "plot"){

  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  plot_data <-
    data %>%
    group_by(Date) %>%
    summarise(n = n_distinct(PersonId), .groups = "drop_last") %>%
    ungroup()

  if(return == "plot"){
    plot_data %>%
      ggplot(aes(x = Date, y = n)) +
      geom_line(size = 1) +
      labs(title = "Population over time",
           subtitle = "Unique licensed population by week",
           caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End)) +
      ylab("Employee count") +
      xlab("Date") +
      scale_y_continuous(labels = round, limits = c(0,NA)) +
      theme_wpa_basic()

  } else if(return == "table"){
    plot_data
  } else {
    stop("Please enter a valid input for `return`.")
  }
}
