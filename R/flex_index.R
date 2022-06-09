# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Compute a Flexibility Index based on the Hourly Collaboration Query
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Pass an Hourly Collaboration query and compute a Flexibility Index for the
#' entire population. The Flexibility Index is a quantitative measure of the
#' freedom for employees to work at a time of their choice.
#'
#' @details
#' The **Flexibility Index** is a metric that has been developed to quantify and
#' measure flexibility using behavioural data from Viva Insights. Flexibility
#' here refers to the freedom of employees to adopt a working arrangement of
#' their own choice, and more specifically refers to **time** flexibility
#' (_whenever_ I want) as opposed to **geographical** flexibility (_wherever_ I
#' want).
#'
#' The **Flexibility Index** is a score between 0 and 1, and is calculated based
#' on three component measures:
#'
#' - `ChangeHours`: this represents the freedom to define work start and end
#' time. Teams that embrace flexibility allow members to start and end their
#' workday at different times.
#'
#' - `TakeBreaks`: this represents the freedom define one's own schedule. In
#' teams that embrace flexibility, some members will choose to organize / split
#' their day in different ways (e.g. take a long lunch-break, disconnect in the
#' afternoon and reconnect in the evening, etc.).
#'
#' - `ControlHours`: this represents the freedom to switch off. Members who
#' choose alternative arrangements should be able to maintain a workload that is
#' broadly equivalent to those that follow standard arrangements.
#'
#' The **Flexibility Index** returns with one single score for each person-week,
#' plus the **three** sub-component binary variables (`TakeBreaks`,
#' `ChangeHours`, `ControlHours`). At the person-week level, each score can only
#' have the values 0, 0.33, 0.66, and 1. The Flexibility Index should only be
#' interpreted as a **group** of person-weeks, e.g. the average Flexibility
#' Index of a team of 6 over time, where the possible values would range from 0
#' to 1.
#'
#' @section Context:
#'   The central feature of flexible working arrangements is
#'   that it is the employee rather the employer who chooses the working
#'   arrangement. _Observed flexibility_ serves as a proxy to assess whether a
#'   flexible working arrangement are in place. The Flexibility Index is an
#'   attempt to create such a proxy for quantifying and measuring flexibility,
#'   using behavioural data from Viva Insights.
#'
#' @section Recurring disconnection time:
#'   The key component of `TakeBreaks` in the Flexibility Index is best
#'   interpreted as 'recurring disconnection time'. This denotes an hourly block
#'   where there is consistently no activity occurring throughout the week. Note
#'   that this applies a stricter criterion compared to the common definition of
#'   a break, which is simply a time interval where no active work is being
#'   done, and thus the more specific terminology 'recurring disconnection time'
#'   is preferred.
#'
#' @param data Hourly Collaboration query to be passed through as data frame.
#'
#' @param hrvar A string specifying the HR attribute to cut the data by.
#'   Defaults to NULL. This only affects the function when "table" is returned.
#'
#' @param signals Character vector to specify which collaboration metrics to
#'   use:
#'   - a combination of signals, such as `c("email", "IM")` (default)
#'   - `"email"` for emails only
#'   - `"IM"` for Teams messages only
#'   - `"unscheduled_calls"` for Unscheduled Calls only
#'   - `"meetings"` for Meetings only
#'
#' @param active_threshold A numeric value specifying the minimum number of
#'   signals to be greater than in order to qualify as _active_. Defaults to 0.
#'
#' @param start_hour A character vector specifying starting hours, e.g. `"0900"`
#'
#' @param end_hour A character vector specifying end hours, e.g. `"1700"`
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"data"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @param plot_method Character string for determining which plot to return.
#'   - `"sample"` plots a sample of ten working pattern
#'   - `"common"` plots the ten most common working patterns
#'   - `"time"` plots the Flexibility Index for the group over time
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: 'ggplot' object. A random of ten working patterns are displayed,
#'   with diagnostic data and the Flexibility Index shown on the plot.
#'   - `"data"`: data frame. The original input data appended with the
#'   Flexibility Index and the component scores. Can be used with
#'   `plot_flex_index()` to recreate visuals found in `flex_index()`.
#'   - `"table"`: data frame. A summary table for the metric.
#'
#' @import dplyr
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @examples
#' # Create a sample small dataset
#' orgs <- c("Customer Service", "Financial Planning", "Biz Dev")
#' em_data <- em_data[em_data$Organization %in% orgs, ]
#'
#' # Examples of how to test the plotting options individually
#' # Sample of 10 work patterns
#' em_data %>%
#'   flex_index(return = "plot", plot_method = "sample")
#'
#' # 10 most common work patterns
#' em_data %>%
#'   flex_index(return = "plot", plot_method = "common")
#'
#' # Plot Flexibility Index over time
#' em_data %>%
#'   flex_index(return = "plot", plot_method = "time")
#'
#' # Return a summary table with the computed Flexibility Index
#' em_data %>%
#'   flex_index(hrvar = "Organization", return = "table")
#'
#' @section Returning the raw data:
#' The raw data containing the computed Flexibility Index can be returned with
#' the following:
#' ```
#' em_data %>%
#'   flex_index(return = "data")
#' ```
#'
#' @family Working Patterns
#'
#' @export
flex_index <- function(data,
                       hrvar = NULL,
                       signals = c("email", "IM"),
                       active_threshold = 0,
                       start_hour = "0900",
                       end_hour = "1700",
                       return = "plot",
                       plot_method = "common"){

  ## Bindings for variables
  TakeBreaks <- NULL
  ChangeHours <- NULL
  ControlHours <- NULL
  FlexibilityIndex <- NULL
  Signals_Break_hours <- NULL

  ## Make sure data.table knows we know we're using it
  .datatable.aware = TRUE

  ## Save original
  start_hour_o <- start_hour
  end_hour_o <- end_hour

  ## Coerce to numeric, remove trailing zeros
  start_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = start_hour))
  end_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = end_hour))
  norm_span <- end_hour - start_hour

  ## convert to data.table
  data2 <-
    data %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    data.table::as.data.table() %>%
    data.table::copy()

  ## Text replacement only for allowed values
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

  ## Signals sent by Person and Date
  signals_df <-
    data2 %>%
    .[, c("PersonId", "Date")] %>%
    cbind(signal_cols)

  ## Signal label
  sig_label <- ifelse(length(signal_set) > 1, "Signals_sent", signal_set)

  ## Create binary variable 0 or 1
  num_cols <- names(which(sapply(signals_df, is.numeric))) # Get numeric columns

  ## Create Signals Total and binary values
  signals_df <-
    signals_df %>%
    data.table::as.data.table() %>%
    # active_threshold: minimum signals to qualify as active
    .[, (num_cols) := lapply(.SD, function(x) ifelse(x > active_threshold, 1, 0)), .SDcols = num_cols] %>%
    .[, ("Signals_Total") := apply(.SD, 1, sum), .SDcols = input_var]

  ## Classify PersonId-Signal data by time of day

  WpA_classify <-
    signals_df %>%
    tidyr::gather(!!sym(sig_label), sent, -PersonId, -Date, -Signals_Total) %>%
    data.table::as.data.table()

  WpA_classify[, StartEnd := gsub(pattern = "[^[:digit:]]", replacement = "", x = get(sig_label))]
  WpA_classify[, Start := as.numeric(substr(StartEnd, start = 1, stop = 2))]
  WpA_classify[, End := as.numeric(substr(StartEnd, start = 3, stop = 4))]
  WpA_classify[, Before_start := Start < (start_hour)] # Earlier than start hour
  WpA_classify[, After_end := End > (end_hour)] # Later than  start hour
  WpA_classify[, Within_hours := (Start >= start_hour & End <= end_hour)]
  WpA_classify[, HourType := NA_character_]
  WpA_classify[After_end == TRUE, HourType := "After_end"]
  WpA_classify[Before_start == TRUE, HourType := "Before_start"]
  WpA_classify[Within_hours == TRUE, HourType := "Within_hours"]

  WpA_classify <-
    WpA_classify[, c("PersonId", "Date", "Signals_Total", "HourType", "sent")] %>%
    .[, .(sent = sum(sent)), by = c("PersonId", "Date", "Signals_Total", "HourType")] %>%
    tidyr::spread(HourType, sent) %>%
    left_join(WpA_classify %>%   ## Calculate first and last activity for day_span
                filter(sent>0) %>%
                group_by(PersonId,Date) %>%
                summarise(First_signal = min(Start),
                          Last_signal = max(End),
                          .groups = "drop_last"),
              by = c("PersonId","Date")) %>%
    mutate(Day_Span = Last_signal-First_signal,
           Signals_Break_hours = Day_Span-Signals_Total) %>%
    select(-Signals_Total)

  ## hrvar treatment
  if(is.null(hrvar)){

    hr_dt <- data2[, c("PersonId", "Date")]
    hr_dt <- hr_dt[, Total := "Total"]
    hrvar <- "Total"
    hr_dt <- as.data.frame(hr_dt)

  } else {

    temp_str <- c("PersonId", "Date", hrvar)

    # hr_dt <- data2[, ..temp_str] # double dot prefix

    hr_dt <-
      data2 %>%
      as.data.frame() %>%
      select(temp_str)
  }


  ## Bind calculated columns with original Signals df
  calculated_data <-
    WpA_classify %>%
    left_join(signals_df, by = c("PersonId","Date")) %>%
    left_join(hr_dt, by = c("PersonId","Date")) %>%
    filter(Signals_Total >= 3) %>% # At least 3 signals required

    ## Additional calculations for Flexibility Index
    mutate(TakeBreaks = (Signals_Break_hours > 0),
           ChangeHours = (First_signal != start_hour),
           ControlHours = (Day_Span <= norm_span)) %>%
    mutate(FlexibilityIndex = select(., TakeBreaks, ChangeHours, ControlHours) %>%
             apply(1, mean))

  ## Plot return
  sig_label_ <- paste0(sig_label, "_")

  ## Summary Table for Return
  ## Applies groups
  returnTable <-
    calculated_data %>%
    group_by(!!sym(hrvar)) %>%
    summarise_at(vars(TakeBreaks, ChangeHours, ControlHours),
                 ~mean(.), .groups = "drop_last") %>%
    mutate(FlexibilityIndex =
             select(., TakeBreaks, ChangeHours, ControlHours) %>%
             apply(1, mean))

  ## Main plot

  if(return == "plot"){

    plot_flex_index(data = calculated_data,
                    sig_label = sig_label_,
                    start_hour = start_hour,
                    end_hour = end_hour,
                    method = plot_method)

  } else if(return == "data"){

    calculated_data

  } else if(return == "table"){

    returnTable

  } else {

    stop("Check input for `return`.")

  }
}
