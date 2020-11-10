# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Manager 1:1 Time Trend
#'
#' @description
#' Provides a week by week view of scheduled manager 1:1 Time.
#' By defualt returns a week by week heatmap, highlighting the points in time with most activity.
#' Additional options available to return a summary table.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization" but accepts any character vector (e.g. "LevelDesignation")
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Managerial Relations
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

one2one_trend <- function(data, hrvar = "Organization", mingroup=5, return = "plot"){

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  myTable <-
    data %>%
    mutate(Date=as.Date(Date, "%m/%d/%Y")) %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    select(PersonId, Date, group, Meeting_hours_with_manager_1_on_1) %>%
    group_by(group) %>%
    mutate(Employee_Count = n_distinct(PersonId)) %>%
    filter(Employee_Count >= mingroup)  # Keep only groups above privacy treshold

  myTable <-
    myTable %>%
    group_by(Date, group) %>%
    summarize(Employee_Count=mean(Employee_Count),
              Minutes_with_manager_1_on_1 = mean(Meeting_hours_with_manager_1_on_1 * 60))

  myTable_plot <-
    myTable %>%
    select(Date, group, Minutes_with_manager_1_on_1)

  myTable_return <- myTable_plot %>% spread(Date, Minutes_with_manager_1_on_1)

  plot_object <-
    myTable_plot %>%
    ggplot(aes(x =Date , y = group , fill = Minutes_with_manager_1_on_1)) +
    geom_tile(height=.5) +
    scale_fill_gradient(name="Minutes", low = "white", high = "red") +
    theme_wpa_basic() +
    labs(title = "Time with Manager",
         subtitle = paste("Average scheduled 1:1 meeting minutes by", tolower(hrvar))) +
    xlab("Date") +
    ylab(hrvar) +
    labs(caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End))

  if(return == "table"){

    myTable_return %>%
      as_tibble() %>%
      return()

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}





