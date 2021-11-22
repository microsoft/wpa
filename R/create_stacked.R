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
#' @param percent Logical value to determine whether to show labels as
#'   percentage signs. Defaults to `FALSE`.
#' @param plot_title String. Option to override plot title.
#' @param plot_subtitle String. Option to override plot subtitle.
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
#'                  return = "plot",
#'                  rank = "ascending")
#'
#' sq_data %>%
#'   create_stacked(hrvar = "FunctionType",
#'                  metrics = c("Meeting_hours",
#'                              "Email_hours",
#'                              "Call_hours",
#'                              "Instant_Message_hours"),
#'                  return = "table")
#'
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
                           percent = FALSE,
                           plot_title = "Collaboration Hours",
                           plot_subtitle = paste("Average by", tolower(camel_clean(hrvar))),
                           legend_lab = NULL,
                           rank = "descending",
                           xlim = NULL,
                           text_just = 0.5,
                           text_colour = "#FFFFFF"
                           ){

  ## Check inputs
  required_variables <- c("Date",
                          metrics,
                          "PersonId")

  ## Error message if variables are not present
  ## Nothing happens if all present
  data %>%
    check_inputs(requirements = required_variables)

  ## Handle `legend_lab`
  if(is.null(legend_lab)){
    legend_lab <- gsub("_", " ", metrics)
  }

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
  if (is.null(xlim)) {
  location <- max(myTable_legends$Total)
  }
  else if(is.numeric(xlim)) {
  location <- xlim
  }
  else {
     stop("Invalid return to `xlim`")
   }

  ## Remove max from axis labels ------------------------------------------
 max_blank <- function(x){
   as.character(
     c(
       x[1:length(x) - 1],
       "")
   )
 }

 ## Remove max from axis labels, but with percentages ---------------------
 max_blank_percent <- function(x){

   x <- scales::percent(x)

   as.character(
     c(
       x[1:length(x) - 1],
       "")
   )
 }

 invert_mean <- function(x){
   mean(x) * -1
 }

  ## Create plot -----------------------------------------------------------

 plot_object <-
   plot_table %>%
   filter(Metric != "Total") %>%
   mutate(Metric = factor(Metric, levels = rev(metrics))) %>%
   group_by(group, Metric) %>%
   summarise_at(vars(Value), ~mean(.)) %>%
   # Conditional ranking based on `rank` argument
   { if(is.null(rank)){
     ggplot(., aes(x = group, y = Value, fill = Metric))
   } else if(rank == "descending"){
     ggplot(., aes(x = stats::reorder(group, Value, mean), y = Value, fill = Metric))
   } else if(rank == "ascending"){
     ggplot(., aes(x = stats::reorder(group, Value, invert_mean), y = Value, fill = Metric))
   } else {
     stop("Invalid return to `rank`")
   }
   } +
   geom_bar(position = "stack", stat = "identity") +
   { if(percent == FALSE){
     geom_text(aes(label = round(Value, 1)),
               position = position_stack(vjust = text_just),
               color = text_colour,
               fontface = "bold")
   } else if(percent == TRUE){
     geom_text(aes(label = scales::percent(Value, accuracy = 0.1)),
               position = position_stack(vjust = text_just),
               color = text_colour,
               fontface = "bold")
   }
   } +
   { if(percent == FALSE){
     scale_y_continuous(expand = c(.01, 0),
                        limits = c(0, location * 1.3),
                        labels = max_blank,
                        position = "right")
   } else if(percent == TRUE){
     scale_y_continuous(expand = c(.01, 0),
                        limits = c(0, location * 1.3),
                        labels = max_blank_percent,
                        position = "right")
   }
   } +
   annotate("text",
            x = myTable_legends$group,
            y = location * 1.15,
            label = myTable_legends$Employee_Count,
            size = 3) +
   annotate("rect",
            xmin = 0.5,
            xmax = length(myTable_legends$group) + 0.5,
            ymin = location * 1.05,
            ymax = location * 1.25,
            alpha = .2) +
   annotate(x=length(myTable_legends$group) + 0.8,
            xend=length(myTable_legends$group) + 0.8,
            y = 0,
            yend = location* 1.04,
            colour = "black",
            lwd = 0.75,
            geom = "segment") +
   scale_fill_manual(name="",
                     values = stack_colours,
                     breaks = metrics,
                     labels = legend_lab) +
   coord_flip() +
   theme_wpa_basic() +
   theme(axis.line = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank()) +
   labs(title = plot_title,
        subtitle = plot_subtitle,
        x = hrvar,
        y = "Average weekly hours",
        caption = extract_date_range(data, return = "text"))

  # Return options ---------------------------------------------------------

  if(return == "table"){

    myTableReturn

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }
}



