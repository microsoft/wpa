# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Classify working pattern personas using a rule based algorithm, using
#' the person-average volume-based (pav) method.
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Apply a rule based algorithm to emails or instant messages sent by hour of day.
#' This uses a person-average volume-based (pav) method.
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#'
#' @param return Character vector to specify what to return. Valid options include:
#'   - "plot": returns a bar plot of signal distribution by hour and archetypes (default)
#'   - "data": returns the raw data with the classified archetypes
#'   - "table": returns a summary table of the archetypes
#'   - "plot-area": returns an overlapping area plot
#'
#' @param values Character vector to specify whether to return percentages
#' or absolute values in "data" and "plot". Valid values are:
#'   - "percent": percentage of signals divided by total signals (default)
#'   - "abs": absolute count of signals
#'
#' @param signals Character vector to specify which collaboration metrics to use:
#'   - "email" (default) for emails only
#'   - "IM" for Teams messages only,
#'   - "unscheduled_calls" for Unscheduled Calls only
#'   - "meetings" for Meetings only
#'   - or a combination of signals, such as `c("email", "IM")`
#'
#' @param start_hour A character vector specifying starting hours,
#' e.g. "0900"
#' @param end_hour A character vector specifying starting hours,
#' e.g. "1700"
#'
#' @import dplyr
#' @import tidyselect
#' @import ggplot2
#'
#' @examples
#' \donttest{
#' # Returns a plot by default
#' workpatterns_classify_pav(em_data)
#'
#' # Return summary table
#' workpatterns_classify_pav(em_data, return = "table")
#'
#' # Return an area plot
#' workpatterns_classify_pav(em_data, return = "plot-area")
#' }
#'
#' @family Working Patterns
#'
workpatterns_classify_pav <- function(data,
                                      values = "percent",
                                      signals = "email",
                                      start_hour = "0900",
                                      end_hour = "1700",
                                      return = "plot"){
  ## Coerce to numeric
  start_hour <- as.numeric(sub(pattern = "00$", replacement = "", x = start_hour))
  end_hour <- as.numeric(sub(pattern = "00$", replacement = "", x = end_hour))

  # Text replacement only for allowed values

  if(any(signals %in% c("email", "IM", "unscheduled_calls", "meetings"))){

    signal_set <- gsub(pattern = "email", replacement = "Emails_sent", x = signals) # case-sensitive
    signal_set <- gsub(pattern = "IM", replacement = "IMs_sent", x = signal_set)
    signal_set <- gsub(pattern = "unscheduled_calls", replacement = "Unscheduled_calls", x = signal_set)
    signal_set <- gsub(pattern = "meetings", replacement = "Meetings", x = signal_set)

  } else {

    stop("Invalid input for `signals`.")

  }

  ## Create 24 summed `Signals_sent` columns
  signal_cols <- purrr::map(0:23, ~combine_signals(data, hr = ., signals = signal_set))
  signal_cols <- bind_cols(signal_cols)

  ## Use names for matching
  input_var <- names(signal_cols)

  ## Average signals sent by Person
  signals_df <-
    data %>%
    select(PersonId) %>%
    cbind(signal_cols) %>%
    group_by(PersonId) %>%
    summarise_all(~mean(.))

  ## Signal label
  sig_label <- ifelse(length(signal_set) > 1, "Signals_sent", signal_set)

  ## Normalised pattern data
  ptn_data_norm <-
    signals_df %>%
    mutate(Signals_Total = select(., all_of(input_var)) %>% apply(1, sum)) %>%
    mutate_at(vars(all_of(input_var)), ~./Signals_Total) %>%
    #filter(Signals_Total > 0) %>%
    select(all_of(input_var)) %>%
    mutate_all(~tidyr::replace_na(., 0)) # Replace NAs with 0s


  ## Normalised pattern data
  ptn_data_norm <-
    signals_df %>%
    mutate(Signals_Total = select(., all_of(input_var)) %>% apply(1, sum)) %>%
    mutate_at(vars(all_of(input_var)), ~./Signals_Total) %>%
    #filter(Signals_Total > 0) %>%
    select(PersonId,Signals_Total, all_of(input_var)) %>%
    mutate_at(vars(all_of(input_var)), # Replace NAs with 0s
              ~tidyr::replace_na(., 0))

  ## Classify PersonId-Signal data by time of day
  ptn_data_classify <-
    ptn_data_norm %>%
    tidyr::gather(Signals_sent, Prop, -PersonId,-Signals_Total) %>%
    mutate(StartEnd = gsub(pattern = "[^[:digit:]]", replacement = "", x = Signals_sent),
           Start = as.numeric(substr(StartEnd, start = 1, stop = 2)),
           End = as.numeric(substr(StartEnd, start = 3, stop = 4))) %>%
    mutate(Before_start = (Start < start_hour)) %>% # Earlier than working hours
    mutate(After_end = (End > end_hour)) %>% # Later than working hours
    mutate(Within_hours = (Start >= start_hour & End <= end_hour)) %>%
    mutate(HourType = case_when(Before_start == TRUE ~ "Before_start",
                                After_end == TRUE ~ "After_end",
                                Within_hours == TRUE ~ "Within_hours",
                                TRUE ~ NA_character_)) %>%
    select(PersonId, HourType, Signals_Total, Prop) %>%
    group_by(PersonId,Signals_Total, HourType) %>%
    summarise(Prop = sum(Prop)) %>%
    tidyr::spread(HourType, Prop) %>%
    ungroup()

  ptn_data_personas <-
  ptn_data_classify %>%
    mutate(Personas =
             case_when(Signals_Total<10~ "Absent",
               Before_start >= .15 & Within_hours < 0.70 & After_end < 0.15 ~ "Extended Hours\n- Morning",
               Before_start < 0.15 & Within_hours < 0.70 & After_end >= 0.15 ~ "Extended Hours\n- Evening",
               Within_hours < 0.3 ~ "Overnight workers",
               Within_hours >= .7 ~ "Standard Hours",
               Before_start >= 0.15 & Within_hours < 0.70 & After_end >= 0.15 ~ "Always On",
               TRUE ~ "Unclassified"))

  ## Percentage vs Absolutes
  if(values == "percent"){

    # bind cut tree to data frame
    ptn_data_final <-
      ptn_data_personas %>%
      left_join(ptn_data_norm, by = "PersonId")

    } else if(values == "abs"){

      ptn_data_final <-
        ptn_data_personas %>%
        left_join(signals_df, by = "PersonId")

    } else {

      stop("Invalid `values` input. Please either input 'percent' or 'abs'.")

    }

  ## Return

  if(return == "data"){

    return(ptn_data_final)

    } else if(return == "plot"){

      plot <-
        plot_signal_clust(ptn_data_final,
                          group_label = "Personas",
                          type = "bar",
                          sig_label = sig_label)
      return(plot)

    } else if(return == "plot-area"){

      plot <-
        plot_signal_clust(ptn_data_final,
                          group_label = "Personas",
                          type = "area",
                          sig_label = sig_label)

      return(plot)

    } else if (return == "table"){

      ## Count table
      count_tb <-
        ptn_data_final %>%
        group_by(Personas) %>%
        summarise(n = n()) %>%
        mutate(prop = n / sum(n))

      ## Summary statistics
      sums_tb <-
        ptn_data_final %>%
        run_sum_hr(group_label = "Personas",
                   sig_label = sig_label)

      ## Time slots
      times_tb <-
        ptn_data_final %>%
        run_hour_splits(start_hour = start_hour,
                        end_hour = end_hour,
                        group_label = "Personas")

      count_tb %>%
        left_join(sums_tb, by = "Personas") %>%
        left_join(times_tb, by = "Personas") %>%
        return()


    } else {

      stop("Invalid input for `return`.")

      }
}
