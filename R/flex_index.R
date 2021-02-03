#' @title Compute a Flexibility Index based on the Hourly Collaboration Query
#'
#' @description Pass an Hourly Collaboration query and compute a Flexibility Index for a group
#'
#' @param data Hourly Collaboration query to be passed through as data frame.
#' @param hrvar A string specifying the HR attribute to cut the data by.
#' Defaults to NULL. This only affects the function when "table" is returned.
#' @param signals Character vector to specify which collaboration metrics to use:
#' You may use "email" for emails only, "IM" for Teams messages only,
#' or a combination of the two `c("email", "IM")` (default).
#' @param active_threshold A numeric value specifying the minimum number of signals to be greater than in
#' order to qualify as _active_. Defaults to 0.
#' @param start_hour A character vector specifying starting hours,
#' e.g. "0900"
#' @param end_hour A character vector specifying end hours,
#' e.g. "1700"
#' @param return Character vector to specify what to return.
#' Valid options include "plot" (default), "data", and "table".
#' When returning a plot, a random of ten working patterns are displayed, with diagnostic data and the Flexibility
#' Index shown on the plot.
#' @param plot_method Character string for determining which plot to return.
#' Options include "sample", "common", and "time". "sample"
#' plots a sample of ten working patterns; "common" plots the ten most common
#' working patterns; "time" plots the Flexibility Index for the group over time.
#'
#' @import dplyr
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @examples
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
#' \dontrun{
#' # Return the raw data with the computed Flexibility Index
#' em_data %>%
#'   flex_index(return = "data")
#'
#'
#' }
#' @family Work Patterns
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

  ## Select input variable names
  if("email" %in% signals & "IM" %in% signals){

    ## Create 24 summed `Signals_sent` columns
    signal_cols <-
      purrr::map(0:23, ~wpa::combine_signals(data2, hr = .)) %>%
      dplyr::bind_cols()

    ## Use names for matching
    input_var <- names(signal_cols)

    ## Signals sent by Person and date
    signals_df <-
      data2 %>%
      .[, c("PersonId", "Date")] %>%
      cbind(signal_cols)

    ## Signal label
    sig_label <- "Signals_sent"

  } else if(signals == "IM"){

    match_index <- grepl(pattern = "^IMs_sent", x = names(data2))
    input_var <- names(data2)[match_index]
    input_var2 <- c("PersonId", "Date", input_var)

    ## signals sent by Person and date
    signals_df <-
      data2 %>%
      .[, ..input_var2]

    sig_label <- "IMs_sent"


  } else if(signals == "email"){

    match_index <- grepl(pattern = "^Emails_sent", x = names(data2))
    input_var <- names(data2)[match_index]
    input_var2 <- c("PersonId", "Date", input_var)

    ## signals sent by Person and date
    signals_df <-
      data2 %>%
      .[, ..input_var2]

    sig_label <- "Emails_sent"

  } else {

    stop("Invalid input for `signals`.")

  }


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
    summarise_at(vars(TakeBreaks, ChangeHours, ControlHours), ~mean(.), .groups = "drop_last") %>%
    mutate(FlexibilityIndex = select(., TakeBreaks, ChangeHours, ControlHours) %>% apply(1, mean))

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
