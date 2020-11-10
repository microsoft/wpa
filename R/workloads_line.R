# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Workloads Time Trend - Line Chart
#'
#' @description
#' Provides a week by week view of 'Work Week Span', visualised as line charts.
#' By default returns a line chart for collaboration hours,
#' with a separate panel per value in the HR attribute.
#' Additional options available to return a summary table.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#'  but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Workloads
#'
#' @examples
#'
#' ## Return a line plot
#' workloads_line(sq_data, hrvar = "LevelDesignation")
#'
#'
#' ## Return a table
#' workloads_line(sq_data, hrvar = "LevelDesignation", return = "table")
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

workloads_line <- function(data,
                           hrvar = "Organization",
                           mingroup=5,
                           return = "plot"){

  ## Check inputs
  required_variables <- c("Date",
                          "Workweek_span",
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  myTable <-
    data %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    select(PersonId, Date, group, Workweek_span) %>%
    group_by(group) %>%
    mutate(Employee_Count = n_distinct(PersonId)) %>%
    filter(Employee_Count >= mingroup)  # Keep only groups above privacy threshold

  myTable <-
    myTable %>%
    group_by(Date, group) %>%
    summarize(Employee_Count = mean(Employee_Count),
              Workweek_span = mean(Workweek_span))

  ## Data frame to return
  myTable_return <-
    myTable %>%
    select(Date, group, Workweek_span) %>%
    spread(Date, Workweek_span)

  ## Data frame for creating plot
  myTable_plot <-
    myTable %>%
    select(Date, group, Workweek_span) %>%
    group_by(Date, group) %>%
    summarise_at("Workweek_span", ~mean(., na.rm = TRUE)) %>%
    ungroup()


  plot_object <-
    myTable_plot %>%
    ggplot(aes(x = Date, y = Workweek_span)) +
    geom_line(colour = "#203864") +
    facet_wrap(.~group) +
    scale_fill_gradient(name="Hours", low = "white", high = "red") +
    theme_classic() +
    theme(plot.title = element_text(color = "grey40",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(size = 14),
          strip.background = element_rect(color = "#0076d7",
                                          fill = "#0076d7"),
          strip.text = element_text(size = 10,
                                    colour = "#FFFFFF",
                                    face = "bold"),
          axis.text = element_text(size = 8, face = "bold"),
          axis.line = element_line(colour = "grey40"),
          legend.position = "right",
          legend.justification = "right",
          legend.title=element_text(size = 10),
          legend.text=element_text(size = 10)) +
    labs(title = "Length of Week",
         subtitle = paste("Average Work Week Span by", hrvar)) +
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
