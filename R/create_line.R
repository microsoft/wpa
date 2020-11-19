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
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric Character string containing the name of the metric,
#' e.g. "Collaboration_hours"
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
#' @importFrom tidyselect all_of
#'
#' @family Flexible
#'
#' @examples
#' \dontrun{
#' sq_data %>% create_line(metric = "Email_hours", return = "plot")
#'
#' sq_data %>% create_line(metric = "Collaboration_hours", return = "plot")
#'
#' sq_data %>% create_line(metric = "Workweek_span", hrvar = "LevelDesignation")
#' }
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export
create_line <- function(data,
                        metric,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot"){

  ## Check inputs
  required_variables <- c("Date",
                          metric,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)


  ## Clean metric name
  clean_nm <- gsub(pattern = "_", replacement = " ", x = metric)

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


  plot_object <-
    myTable_plot %>%
    ggplot(aes(x = Date, y = !!sym(metric))) +
    geom_line(colour = "grey40") +
    facet_wrap(.~group) +
    scale_fill_gradient(name="Hours", low = "white", high = "red") +
    theme_classic() +
    theme(plot.title = element_text(color = "grey40",
                                    face = "bold",
                                    size = 18),
          plot.subtitle = element_text(size = 14),
          strip.background = element_rect(color = "grey40",
                                          fill = "grey40"),
          strip.text = element_text(size = 10,
                                    colour = "#FFFFFF",
                                    face = "bold"),
          axis.text = element_text(size = 8, face = "bold"),
          axis.line = element_line(colour = "grey40"),
          legend.position = "right",
          legend.justification = "right",
          legend.title=element_text(size = 10),
          legend.text=element_text(size = 10)) +
    labs(title = clean_nm,
         subtitle = paste("Total",
                          tolower(clean_nm),
                          "by",
                          camel_clean(hrvar))) +
    xlab("Date") +
    ylab("Weekly hours") +
    labs(caption = extract_date_range(data, return = "text"))

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
