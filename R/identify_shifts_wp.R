# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify shifts based on binary activity
#'
#' @description
#' This function uses the Hourly Collaboration query and computes binary
#' activity to identify the 'behavioural' work shift. This is a distinct method
#' to `identify_shifts()`, which instead uses outlook calendar settings for
#' start and end time of work day to identify work shifts. The two methods can
#' be compared to gauge the accuracy of existing Outlook settings.
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @inheritParams workpatterns_classify_bw
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object. A bar plot for the weekly count of shifts.
#'   - `"table"`: data frame. A summary table for the count of shifts.
#'   - `"data`: data frame. Input data appended with the following columns:
#'     - `Start`
#'     - `End`
#'     - `DaySpan`
#'     - `Shifts`
#'
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @family Work Patterns
#'
#' @examples
#' # Return plot
#' em_data %>% identify_shifts_wp()
#'
#' # Return table
#' em_data %>% identify_shifts_wp(return = "table")
#'
#' @export
identify_shifts_wp <- function(data,
                               signals = c("email",
                                           "IM"),
                               active_threshold = 1,
                               start_hour = 9,
                               end_hour = 17,
                               return = "plot"){

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

  ## Create 24 summed `Signals_sent` columns
  signal_cols <-
    purrr::map(0:23,
               ~combine_signals(data,
                                hr = .,
                                signals = signal_set)) %>%
    dplyr::bind_cols()

  ## Use names for matching
  input_var <- names(signal_cols)

  ## Signals sent by Person and date
  signals_df <-
    data %>%
    .[, c("PersonId", "Date")] %>%
    cbind(signal_cols)

  ## Signal label
  sig_label <- "Signals_sent"

  ## Create binary variable 0 or 1
  num_cols <- names(which(sapply(signals_df, is.numeric))) # Get numeric columns

  signals_df <-
    signals_df %>%
    data.table::as.data.table() %>%
    # Minimum number of signals to qualify as working
    .[, (num_cols) := lapply(.SD, function(x) ifelse(x > active_threshold, 1, 0)), .SDcols = num_cols]

  ## Computed data frame with the following columns
  ##   - Start
  ##   - End
  ##   - DaySpan
  ##   - Shifts
  out_data <-
    signals_df %>%
    as.data.frame() %>%
    pivot_longer(cols = input_var, names_to = "Signals") %>%
    mutate(Hour = gsub(pattern = "Signals_sent_", replacement = "", x = Signals)) %>%
    mutate(Hour = gsub(pattern = "_.+", replacement = "", x = Hour) %>%
             as.numeric()) %>%
    select(PersonId, Date, Hour, value) %>%
    filter(value == 1) %>%
    arrange(Hour) %>%
    group_by(PersonId, Date) %>%
    summarise(Start = first(Hour),
              End = last(Hour)) %>%
    ungroup() %>%
    mutate(Shifts = paste0(Start, ":00-", End, ":00")) %>%
    mutate(DaySpan = End - Start)

  if(return == "data"){

    out_data

  } else if(return == "table"){

    out_data %>%
      group_by(Shifts, DaySpan) %>%
      summarise(WeekCount = n(),
                PersonCount = n_distinct(PersonId)) %>%
      arrange(desc(WeekCount)) %>%
      ungroup() %>%
      mutate(`Week%` = WeekCount / sum(WeekCount, na.rm = TRUE),
             `Person%` = PersonCount / sum(PersonCount, na.rm = TRUE))

  } else if(return == "plot"){

    out_data %>%
      group_by(Shifts) %>%
      summarise(WeekCount = n()) %>%
      arrange(desc(WeekCount)) %>%
      utils::head(10) %>%
      create_bar_asis(group_var = "Shifts",
                      bar_var = "WeekCount",
                      title = "Most frequent shifts",
                      subtitle = "Showing top 10 only",
                      caption = extract_date_range(data, return = "text"),
                      ylab = "Shifts",
                      xlab = "Frequency")
  }
}
