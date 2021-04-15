# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Horizontal stacked bar plot for any metric
#'
#' @description
#' Creates a sum total calculation using selected metrics,
#' where the typical use case is to create different definitions of
#' collaboration hours.
#' Returns a stacked bar plot by default.
#' Additional options available to return a summary table.
#'
#' @template spq-params
#' @param metrics A character vector to specify variables to be used
#' in calculating the "Total" value, e.g. c("Meeting_hours", "Email_hours").
#' The order of the variable names supplied determine the order in which they
#' appear on the stacked plot.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#' @param stack_colours
#' A character vector to specify the colour codes for the stacked bar charts.
#' @param plot_title An option to override plot title.
#' @param plot_subtitle An option to override plot subtitle.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family Visualization
#' @family Flexible
#'
#' @return
#' Returns a 'ggplot' object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @examples
#' sq_data %>%
#'   create_stacked(hrvar = "LevelDesignation",
#'                  metrics = c("Meeting_hours", "Email_hours"),
#'                  return = "plot")
#'
#' sq_data %>%
#'   create_stacked(hrvar = "FunctionType",
#'                  metrics = c("Meeting_hours",
#'                              "Email_hours",
#'                              "Call_hours",
#'                              "Instant_Message_hours"),
#'                  return = "plot")
#'
#' sq_data %>%
#'   create_stacked(hrvar = "FunctionType",
#'                  metrics = c("Meeting_hours",
#'                              "Email_hours",
#'                              "Call_hours",
#'                              "Instant_Message_hours"),
#'                  return = "table")
#' @export
create_stacked <- function(data,
                           hrvar = "Organization",
                           metrics = c("Meeting_hours",
                                       "Email_hours"),
                           mingroup = 5,
                           return = "plot",
                           stack_colours = c("#1d627e",
                                             "#34b1e2",
                                             "#b4d5dd",
                                             "#adc0cb"),
                           plot_title = "Collaboration Hours",
                           plot_subtitle = paste("Average by", tolower(camel_clean(hrvar)))){

  ## Check inputs
  required_variables <- c("Date",
                          metrics,
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

  n_count <-
    data %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    group_by(group) %>%
    summarise(Employee_Count = n_distinct(PersonId))

  ## Person level table
  myTable <-
    data %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    select(PersonId, group, metrics) %>%
    group_by(PersonId, group) %>%
    summarise_at(vars(metrics), ~mean(.)) %>%
    ungroup() %>%
    mutate(Total = select(., metrics) %>% apply(1, sum)) %>%
    left_join(n_count, by = "group") %>%
    # Keep only groups above privacy threshold
    filter(Employee_Count >= mingroup)

  myTableReturn <-
    myTable %>%
    group_by(group) %>%
    summarise_at(vars(metrics, Total), ~mean(.)) %>%
    left_join(n_count, by = "group")

  plot_table <-
    myTable %>%
    select(PersonId, group, metrics, Total) %>%
    gather(Metric, Value, -PersonId, -group)

  totalTable <-
    plot_table %>%
    filter(Metric == "Total") %>%
    group_by(group) %>%
    summarise(Total = mean(Value))

  myTable_legends <-
    n_count %>%
    filter(Employee_Count >= mingroup) %>%
    mutate(Employee_Count = paste("n=",Employee_Count)) %>%
    left_join(totalTable, by = "group")

  ## Get maximum value
  location <- max(myTable_legends$Total)

  ## Remove max from axis labels
 max_blank <- function(x){
   as.character(
     c(
       x[1:length(x) - 1],
       "")
   )
 }
 
  ## Create plot

  plot_object <-
    plot_table %>%
    filter(Metric != "Total") %>%
    mutate(Metric = factor(Metric, levels = rev(metrics))) %>%
    group_by(group, Metric) %>%
    summarise_at(vars(Value), ~mean(.)) %>%
    ggplot(aes(x = stats::reorder(group, Value, mean), y = Value, fill = Metric)) +
    geom_bar(position = "stack", stat = "identity") +
    geom_text(aes(label = round(Value, 1)),
              position = position_stack(vjust = 0.5),
              color = "#FFFFFF",
              fontface = "bold") +
    scale_y_continuous(expand = c(.01, 0), limits = c(0, location * 1.25),labels=max_blank, position = "right") +
    annotate("text",
             x = myTable_legends$group,
             y = location * 1.15,
             label = myTable_legends$Employee_Count,
			 size=3) +
    annotate("rect", xmin = 0.5, xmax = length(myTable_legends$group) + 0.5, ymin = location * 1.05, ymax = location * 1.25, alpha = .2) +
	annotate(x=length(myTable_legends$group) + 0.8, xend=length(myTable_legends$group) + 0.8, y=0, yend=location* 1.04, colour="black", lwd=0.75, geom="segment") +
    scale_fill_manual(name="",
                      values = stack_colours,
                      breaks = metrics,
                      labels = gsub("_", " ", metrics)) +
    coord_flip() +
    theme_wpa_basic() +
	theme(axis.line = element_blank(),
		axis.ticks = element_blank(),   
		axis.title = element_blank()) +
    labs(title = plot_title,
         subtitle = plot_subtitle,
         caption = extract_date_range(data, return = "text")) +
    xlab(hrvar) +
    ylab("Average weekly hours")

  if(return == "table"){

    myTableReturn

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}



