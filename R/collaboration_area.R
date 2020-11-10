# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration - Stacked Area Plot
#'
#' @description
#' Provides an overview analysis of 'Weekly Digital Collaboration'.
#' Returns an stacked area plot of Email and Meeting Hours by default.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metrics `Meeting_hours`, `Email_hours`, `Unscheduled_Call_hours`,
#' and `Instant_Message_hours`.
#'
#' @param data A Standard Query dataset in the form of a data frame.
#' A Collaboration assessment dataset may also be provided, in which
#' Unscheduled call hours would be included in the output.
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Collaboration
#'
#' @return
#' Returns a ggplot object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

collaboration_area <- function(data,
                               hrvar = "Organization",
                               mingroup=5,
                               return = "plot"){

  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

  if("Instant_message_hours" %in% names(data)){

    data <- rename(data, Instant_Message_hours = "Instant_message_hours")

  }

  if("Unscheduled_call_hours" %in% names(data)){

    data <- rename(data, Unscheduled_Call_hours = "Unscheduled_call_hours")

  }

  if("Unscheduled_Call_hours" %in% names(data)){

    main_vars <- c("Meeting_hours",
                   "Email_hours",
                   "Instant_Message_hours",
                   "Unscheduled_Call_hours")

  } else {

    main_vars <- c("Meeting_hours",
                   "Email_hours",
                   "Instant_Message_hours")

  }

  myTable <-
    data %>%
    rename(group = !!sym(hrvar)) %>% # Rename HRvar to `group`
    select(PersonId,
           Date,
           group,
           main_vars) %>%
    group_by(Date, group) %>%
    summarise_at(vars(main_vars), ~mean(.)) %>%
    left_join(hrvar_count(data, hrvar, return = "table"),
              by = c("group" = hrvar)) %>%
    rename(Employee_Count = "n") %>%
    filter(Employee_Count >= mingroup) %>%
    ungroup()


  myTable_long <-
    myTable %>%
    select(Date, group, ends_with("_hours")) %>%
    gather(Metric, Hours, -Date, -group) %>%
    mutate(Metric = sub(pattern = "_hours", replacement = "", x = Metric))

  ## Levels
  level_chr <- sub(pattern = "_hours", replacement = "", x = main_vars)

  ## Colour definitions
  colour_defs <-
    c("Meeting" = rgb2hex(174, 239, 168),
      "Email" = rgb2hex(192, 117, 205),
      "Instant_Message" = rgb2hex(113, 200, 234),
      "Unscheduled_Call" = rgb2hex(252, 161, 124))

  colour_defs <- colour_defs[names(colour_defs) %in% level_chr]

  plot_object <-
    myTable_long %>%
    mutate(Metric = factor(Metric, levels = level_chr)) %>%
    ggplot(aes(x = Date, y = Hours, colour = Metric)) +
    geom_area(aes(fill = Metric), alpha = 1.0, position = 'stack') +
    theme_wpa_basic() +
    scale_y_continuous(labels = round) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_colour_manual(values = colour_defs) +
    scale_fill_manual(values = colour_defs) +
    facet_wrap(.~group) +
    labs(title = "Total Collaboration Hours",
         subtitle = paste("Weekly collaboration hours by",  camel_clean(hrvar))) +
    labs(caption = extract_date_range(data, return = "text"))

  if(return == "table"){

    myTable %>%
      as_tibble() %>%
      mutate(Collaboration_hours = select(., main_vars) %>%
               apply(1, sum, na.rm = TRUE))

  } else if(return == "plot"){

    return(plot_object)

  } else {

    stop("Please enter a valid input for `return`.")

  }

}
