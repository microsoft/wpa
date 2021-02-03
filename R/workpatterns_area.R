# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create an area plot of emails and IMs by hour of the day
#'
#' @description
#' Uses the Hourly Collaboration query to produce an area plot of
#' Emails sent and IMs sent attended by hour of the day.
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#'
#' @param hrvar HR Variable by which to split metrics. Accepts a character vector,
#' defaults to "Organization" but accepts any character vector, e.g. "LevelDesignation"
#'
#' @param mingroup Numeric value setting the privacy threshold / minimum group size, defaults to 5.
#'
#' @param signals Character vector to specify which collaboration metrics to use:
#'   - "email" (default) for emails only
#'   - "IM" for Teams messages only
#'   - "unscheduled_calls" for Unscheduled Calls only
#'   - "meetings" for Meetings only
#'   - or a combination of signals, such as `c("email", "IM")`
#'
#' @param return Character vector to specify what to return. Valid options include:
#'   - "plot": returns an overlapping area plot (default)
#'   - "table": returns a summary table
#'
#' @param values Character vector to specify whether to return percentages
#' or absolute values in "data" and "plot". Valid values are:
#'   - "percent": percentage of signals divided by total signals (default)
#'   - "abs": absolute count of signals
#'
#' @param start_hour A character vector specifying starting hours,
#' e.g. "0900"
#'
#' @param end_hour A character vector specifying starting hours,
#' e.g. "1700"
#'
#'
#' @import dplyr
#' @import tidyselect
#' @import ggplot2
#' @importFrom tidyr replace_na
#'
#' @examples
#' ## Return visualization of percentage distribution
#' workpatterns_area(em_data, return = "plot", values = "percent")
#'
#' ## Return visualization of absolute values
#' workpatterns_area(em_data, return = "plot", values = "abs")
#'
#' ## Return a table
#' workpatterns_area(em_data, return = "table")
#'
#' @family Work Patterns
#'
#' @export
workpatterns_area <- function(data,
                              hrvar = "Organization",
                              mingroup = 5,
                              signals = c("email", "IM"),
                              return = "plot",
                              values = "percent",
                              start_hour = "0900",
                              end_hour = "1700"){

  ## Remove case-sensitivity for signals
  signals <- tolower(signals)

  ## Text replacement only for allowed values
  if(any(signals %in% c("email", "im", "unscheduled_calls", "meetings"))){

    signal_set <- gsub(pattern = "email", replacement = "Emails_sent", x = signals) # case-sensitive
    signal_set <- gsub(pattern = "im", replacement = "IMs_sent", x = signal_set)
    signal_set <- gsub(pattern = "unscheduled_calls", replacement = "Unscheduled_calls", x = signal_set)
    signal_set <- gsub(pattern = "meetings", replacement = "Meetings", x = signal_set)

    total_signal_set <- paste0(signal_set, "_total")
    search_set <- paste(paste0("^", signal_set, "_"), collapse = "|")

  } else {

    stop("Invalid input for `signals`.")

  }

  ## Get list of variable names
  ## Size equivalent to size of `signal_set`
  list_signal_set <-
    signal_set %>%
    purrr::map(function(signal_text){

      names(data)[grep(pattern = signal_text, x = names(data))]

    }) %>%
    setNames(nm = total_signal_set) # Use total signals set

  ## Convert as vector
  input_var <- unlist(list_signal_set) %>% as.character()

  ## Date range data frame
  myPeriod <- extract_date_range(data)

  ## Average signals sent by Person
  signals_df <-
    data %>%
    select(PersonId, hrvar, all_of(input_var)) %>%
    rename(group = !!sym(hrvar)) %>%
    group_by(PersonId, group) %>%
    summarise_all(~mean(.)) %>%
    ungroup() %>%
    left_join(data %>%
                rename(group = !!sym(hrvar)) %>%
                group_by(group) %>%
                summarise(Employee_Count = n_distinct(PersonId)),
              by = "group") %>%
    filter(Employee_Count >= mingroup)

  ## Loop and create totals
  ptn_data_norm <- signals_df

  for(i in 1:length(list_signal_set)){

    ## Name of signal
    sig_name <- names(list_signal_set)[i]
    sig_name_all <- list_signal_set[[i]] # individual var names


    ptn_data_norm <-
      ptn_data_norm %>%
      mutate(!!sym(sig_name) := select(., all_of(sig_name_all)) %>% apply(1, sum)) %>%
      mutate_at(vars(sig_name_all), ~./ !!sym(sig_name))

  }

  ## Normalised pattern data
  ptn_data_norm <-
    ptn_data_norm %>%
    select(PersonId, group, all_of(input_var)) %>%
    mutate_all(~tidyr::replace_na(., 0)) # Replace NAs with 0s

  # Percentage vs Absolutes
  if(values == "percent"){

    # Use normalised data
    ptn_data_final <- ptn_data_norm
    abs_true <- FALSE

  } else if(values == "abs"){

    # Use data with direct person-average
    ptn_data_final <- signals_df
    abs_true <- TRUE

  } else {

    stop("Invalid `values` input. Please either input 'percent' or 'abs'.")

  }

  ## Create summary table
  summary_data <-
    ptn_data_final %>%
    group_by(group) %>%
    summarise_at(vars(input_var), ~mean(.)) %>%
    gather(Signals, Value, -group) %>%
    mutate(Hours = sub(pattern = search_set, replacement = "", x = Signals)) %>%
    mutate(Hours = sub(pattern = "_.+", replacement = "", x = Hours)) %>%
    mutate(Hours = as.numeric(Hours)) %>%
    mutate(Signals = sub(pattern = "_\\d.+", replacement = "", x = Signals)) %>%
    spread(Signals, Value)

  ## Create the two-digit zero-padded format
  ## Used in `scale_x_continuous()`
  pad2 <- function(x){
    x <- as.character(x)

    ifelse(nchar(x) == 1, paste0("0", x), x)
  }

  ## Return
  if(return == "data"){

    return(ptn_data_final)

  } else if(return == "plot"){

    start_line <- as.numeric(gsub(pattern = "0", replacement = "", x = start_hour))
    end_line <- as.numeric(gsub(pattern = "0", replacement = "", x = end_hour))

    plot <-
      summary_data %>%
      mutate_at(vars(group), ~as.factor(.)) %>%
      gather(Signals, Value, -group, -Hours) %>%
      ggplot(aes(x = Hours, y = Value, colour = Signals)) +
      geom_line(size = 1) +
      geom_area(aes(fill = Signals), alpha = 0.2, position = 'identity') +
      geom_vline(xintercept = c(start_line, end_line), linetype="dotted") +
      scale_x_continuous(labels = pad2) +
      facet_wrap(.~group) +
      theme_wpa_basic() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle(label = "Distribution of collaboration activities\n by hour of day",
              subtitle = paste("By", camel_clean(hrvar))) +
      labs(caption = paste("Data from week of", myPeriod$Start, "to week of", myPeriod$End)) +
      {if (abs_true) ylab(paste("Collaboration activity\n(absolute)"))
        else ylab(paste("Collaboration activity\n(percentage of daily total)"))
      }

    return(plot)

  } else if(return == "table"){

    ## Count table
    count_tb <-
      ptn_data_final %>%
      group_by(group) %>%
      summarise(n = n_distinct(PersonId))

    summary_data %>%
      left_join(count_tb, by = "group") %>%
      return()

  } else {

    stop("Invalid input for `return`.")

  }
}
