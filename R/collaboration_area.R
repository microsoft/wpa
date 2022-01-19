# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration - Stacked Area Plot
#'
#' @description
#' Provides an overview analysis of Weekly Digital Collaboration.
#' Returns an stacked area plot of Email and Meeting Hours by default.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metrics `Meeting_hours`, `Email_hours`, `Unscheduled_Call_hours`,
#' and `Instant_Message_hours`.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' A Ways of Working assessment dataset may also be provided, in which
#' Unscheduled call hours would be included in the output.
#' @param hrvar HR Variable by which to split metrics, defaults to `NULL`, but
#'   accepts any character vector, e.g. "LevelDesignation". If `NULL` is passed,
#'   the organizational attribute is automatically populated as "Total".
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#'
#' @family Visualization
#' @family Collaboration
#'
#' @examples
#' # Return plot with total (default)
#' collaboration_area(sq_data)
#'
#' # Return plot with hrvar split
#' collaboration_area(sq_data, hrvar = "Organization")
#'
#' # Return summary table
#' collaboration_area(sq_data, return = "table")
#'
#' @return
#' A different output is returned depending on the value passed to the `return` argument:
#'   - `"plot"`: 'ggplot' object. A stacked area plot for the metric.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @export

collaboration_area <- function(data,
                               hrvar = NULL,
                               mingroup=5,
                               return = "plot"){

  ## Handle variable name consistency
  data <- qui_stan_c(data)
  data <- qui_stan_im(data)

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  ## Date cleaning
  data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

  ## Lower case version of column names
  lnames <- tolower(names(data))

  if("unscheduled_call_hours" %in% lnames){

    names(data) <-
      gsub(pattern = "unscheduled_call_hours",
           replacement = "Unscheduled_Call_hours",
           x = names(data),
           ignore.case = TRUE) # Case-insensitive

  }

  ## Exclude metrics if not available as a metric

  check_chr <- c("^Meeting_hours$",
                 "^Email_hours$",
                 "^Instant_Message_hours$",
                 "^Unscheduled_Call_hours$")

  main_vars <-
    names(data)[
    grepl(pattern = paste(check_chr, collapse = "|"),
          x = lnames,
          ignore.case = TRUE)
  ]

  ## Analysis table

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
    c("Meeting" = "#34b1e2",
      "Email" = "#1d627e",
      "Instant_Message" = "#adc0cb",
      "Unscheduled_Call" = "#b4d5dd")

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

#' @rdname collaboration_area
#' @export
collab_area <- collaboration_area
