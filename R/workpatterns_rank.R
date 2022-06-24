# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Create a rank table of working patterns
#'
#' @description
#' Takes in an Hourly Collaboration query and returns a count
#' table of working patterns, ranked from the most common to the
#' least.
#'
#' @param data A data frame containing hourly collaboration data.
#' @param signals Character vector to specify which collaboration metrics to
#'   use:
#'   - `"email"` (default) for emails only
#'   - `"IM"` for Teams messages only
#'   - `"unscheduled_calls"` for Unscheduled Calls only
#'   - `"meetings"` for Meetings only
#'   - or a combination of signals, such as `c("email", "IM")`
#' @param start_hour A character vector specifying starting hours,
#' e.g. "`0900"`
#' @param end_hour A character vector specifying starting hours,
#' e.g. `"1700"`
#' @param top numeric value specifying how many top working patterns to display in plot,
#' e.g. `"10"`
#'
#' @param mode string specifying aggregation method for plot. Valid
#' options include:
#'   - `"binary"`: convert hourly activity into binary blocks. In the plot, each
#'   block would display as solid.
#'   - `"prop"`: calculate proportion of signals in each hour over total signals
#'   across 24 hours, then average across all work weeks. In the plot, each
#'   block would display as a heatmap.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.

#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"plot"`: ggplot object. A plot with the y-axis showing the top ten
#'   working patterns and the x-axis representing each hour of the day.
#'   - `"table"`: data frame. A summary table for the top working patterns.
#'
#' @importFrom data.table ":=" "%like%" "%between%"
#'
#' @examples
#' # Plot by default
#' workpatterns_rank(
#'   data = em_data,
#'   signals = c(
#'     "email",
#'     "IM",
#'     "unscheduled_calls",
#'     "meetings"
#'   )
#'   )
#'
#' # Plot with prop / heatmap mode
#' workpatterns_rank(
#'   data = em_data,
#'   mode = "prop"
#' )
#'
#'
#' @family Visualization
#' @family Working Patterns
#'
#' @export
workpatterns_rank <- function(data,
                              signals = c("email", "IM"),
                              start_hour = "0900",
                              end_hour = "1700",
                              top = 10,
                              mode = "binary",
                              return = "plot"){

  # Make sure data.table knows we know we're using it
  .datatable.aware = TRUE

  ## Save original
  start_hour_o <- start_hour
  end_hour_o <- end_hour

  ## Coerce to numeric, remove trailing zeros
  start_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = start_hour))
  end_hour <- as.numeric(gsub(pattern = "00$", replacement = "", x = end_hour))

  ## convert to data.table
  data2 <-
    data %>%
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
    data.table::as.data.table() %>%
    data.table::copy()

  ## Dynamic input for signals
  ## Text replacement only for allowed values
  if(any(signals %in% c("email", "IM", "unscheduled_calls", "meetings"))){

    signal_set <- gsub(pattern = "email", replacement = "Emails_sent", x = signals) # case-sensitive
    signal_set <- gsub(pattern = "IM", replacement = "IMs_sent", x = signal_set)
    signal_set <- gsub(pattern = "unscheduled_calls", replacement = "Unscheduled_calls", x = signal_set)
    signal_set <- gsub(pattern = "meetings", replacement = "Meetings", x = signal_set)

    ## Create label for plot subtitle
    subtitle_signal <- "signals" # placeholder

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


  ## This should only pick up `Signals_sent_` prefixed columns
  ## This is run on `signals_df`
  num_cols <- names(which(sapply(signals_df, is.numeric))) # Get numeric columns

  if(mode == "binary"){

    ## Summarized table performed on `signals_df` ----------------------------
    ## Section ignoring `signals_df_o`

    signals_df <-
      signals_df %>%
      data.table::as.data.table() %>%
      .[, (num_cols) := lapply(.SD, function(x) ifelse(x > 0, 1, 0)), .SDcols = num_cols] %>%
      .[, list(WeekCount = .N, PersonCount = dplyr::n_distinct(PersonId)), by = input_var]

    myTable_return <- data.table::setorder(signals_df, -PersonCount)

  } else if(mode == "prop"){

    ## Save original `signals_df` before manipulating ------------------------
    ## Rename `Signals_sent` columns to prevent conflict
    signals_df_o <- signals_df %>%
      purrr::set_names(
        nm = gsub(x = names(.),
                  replacement = "_ori_",
                  pattern = "_sent_")
      ) %>%
      cbind(select(signals_df, num_cols)) %>% # duplicate signals
      # Convert `Signals_sent_` prefixed to binary. `Signals_ori_` are intact
      # Create binary variable 0 or 1
      .[, (num_cols) := lapply(.SD, function(x) ifelse(x > 0, 1, 0)), .SDcols = num_cols] %>%
      # Use `mutate()` method
      .[, `:=`(WeekCount = .N,
               PersonCount = dplyr::n_distinct(PersonId),
               Id = .GRP), # group id assignment
        by = num_cols]

    ## 00, 01, 02, etc.
    hours_col <- stringr::str_pad(seq(0,23), width = 2, pad = 0)

    # Wide table showing proportion of signals by hour
    # Ranked descending by `WeekCount`
    wp_prop_tb <-
      signals_df_o %>%
      arrange(desc(WeekCount)) %>%
      dplyr::select(Id, dplyr::contains("_ori_"), WeekCount)  %>%
      purrr::set_names(nm = gsub(
        pattern = ".+_ori_",
        replacement = "",
        x = names(.)
      )) %>%
      purrr::set_names(nm = gsub(
        pattern = "_.+",
        replacement = "",
        x = names(.)
      )) %>%
      # Need aggregation
      .[, Signals_Total := rowSums(.SD), .SDcols = hours_col] %>%
      .[, c(hours_col) := .SD / Signals_Total, .SDcols = hours_col] %>%
      .[, Signals_Total := NULL] %>% # Remove unneeded column
      .[, lapply(.SD, mean, na.rm = TRUE), .SDcols = hours_col, by = list(Id, WeekCount)]


  } else {

    stop("invalid value to `mode`.")

  }


  if(return == "plot" & mode == "binary"){

    ## Plot return
    sig_label_ <- paste0(sig_label, "_")

  	myTable_return <-
  	  myTable_return %>%
  	  arrange(desc(WeekCount)) %>%
  	  mutate(patternRank= 1:nrow(.))

    ## Table for annotation
    myTable_legends <-
      myTable_return %>%
      dplyr::select(patternRank, WeekCount) %>%
      dplyr::mutate(WeekPercentage = WeekCount / sum(WeekCount, na.rm = TRUE),
                    WeekCount = paste0(scales::percent(WeekPercentage, accuracy = 0.1))) %>%
      utils::head(top)

	coverage <-
	  myTable_legends %>%
	  summarize(total = sum(WeekPercentage)) %>%
	  pull(1) %>%
	  scales::percent(accuracy = 0.1)

	myTable_return %>%
	  dplyr::select(patternRank, dplyr::starts_with(sig_label_))  %>%
	  purrr::set_names(nm = gsub(
	    pattern = sig_label_,
	    replacement = "",
	    x = names(.)
	  )) %>%
	  purrr::set_names(nm = gsub(
	    pattern = "_.+",
	    replacement = "",
	    x = names(.)
	  )) %>%
	  plot_hourly_pat(
	    start_hour = start_hour,
	    end_hour = end_hour,
	    legend = myTable_legends,
	    legend_label = "WeekCount",
	    legend_text = paste("Observed", subtitle_signal, "activity"),
	    rows = top,
	    title = "Patterns of digital activity",
	    subtitle = paste(
	      "Hourly activity based on",
	      subtitle_signal,
	      "sent over a week"),
	    caption = paste(
	      "Top",
	      top,
	      "patterns represent",
	      coverage,
	      "of workweeks.\n",
	      extract_date_range(data, return = "text")
	    ),
	    ylab = paste("Top", top, "activity patterns")
	  )

  } else if(return == "plot" & mode == "prop"){


    ## Table for annotation
    myTable_legends <-
      wp_prop_tb %>%
      arrange(desc(WeekCount)) %>%
      mutate(patternRank= 1:nrow(.)) %>%
      dplyr::select(patternRank, WeekCount) %>%
      dplyr::mutate(WeekPercentage = WeekCount / sum(WeekCount, na.rm = TRUE),
                    WeekCount = paste0(scales::percent(WeekPercentage, accuracy = 0.1))) %>%
      utils::head(top)

    ## Coverage
    coverage <-
      myTable_legends %>%
      summarize(total = sum(WeekPercentage)) %>%
      pull(1) %>%
      scales::percent(accuracy = 0.1)


    ## Run plot

    wp_prop_tb %>%
      dplyr::mutate(patternRank = 1:nrow(.)) %>%
      plot_hourly_pat(
        start_hour = start_hour,
        end_hour = end_hour,
        legend = myTable_legends,
        legend_label = "WeekCount",
        legend_text = paste("Observed", subtitle_signal, "activity"),
        rows = top,
        title = "Patterns of digital activity",
        subtitle = paste(
          "Hourly activity based on",
          subtitle_signal,
          "sent over a week"),
        caption = paste(
          "Top",
          top,
          "patterns represent",
          coverage,
          "of workweeks.\n",
          extract_date_range(data, return = "text")
        ),
        ylab = paste("Top", top, "activity patterns")
    )

  } else if(return == "table"){

    dplyr::as_tibble(myTable_return)

  } else if(return == "test"){

    signals_df_o

  } else {

    stop("Invalid `return`")

  }
}
