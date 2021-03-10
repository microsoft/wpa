# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Combine signals from the Hourly Collaboration query
#'
#' @description
#' Takes in an Hourly Collaboration Data, and for each hour sums and aggregates
#' the signals (e.g.`Emails_sent` and `IMs_sent`) in `Signals_sent`. This is an
#' internal function used in the Working Patterns functions.
#'
#' @param data Hourly Collaboration query containing signal variables (e.g.
#'   `Emails_sent_00_01`)
#' @param hr Numeric value between 0 to 23 to iterate through
#' @param signals Character vector for specifying which signal types to combine.
#'   Defaults to `c("Emails_sent", "IMs_sent")`. Other valid values include
#'   `"Unscheduled_calls"` and `"Meetings"`.
#'
#' @details
#' `combine_signals` uses string matching to aggregate columns.
#'
#' @family Support
#'
#' @examples
#' # Demo using simulated variables
#' sim_data <-
#'   data.frame(Emails_sent_09_10 = sample(1:5, size = 10, replace = TRUE),
#'              Unscheduled_calls_09_10 = sample(1:5, size = 10, replace = TRUE))
#'
#' combine_signals(sim_data, hr = 9, signals = c("Emails_sent", "Unscheduled_calls"))
#'
#' @export
combine_signals <- function(data,
                            hr,
                            signals = c("Emails_sent", "IMs_sent")){

  if(!is.numeric(hr) | hr < 0 | hr > 23){

    stop("Check inputs for `hr` in `combine_signals()`")

  }

  # End hour
  hr_two <- hr + 1

  # String pad to two digits
  hr1 <- ifelse(nchar(hr) == 1, paste0(0, hr), hr)
  hr2 <- ifelse(nchar(hr_two) == 1, paste0(0, hr_two), hr_two)

  # Create string vectors
  # Use original supplied string if length of signals == 1
  if(length(signals) == 1){

    full_string <- paste0(signals, "_", hr1, "_", hr2)

  } else {

    full_string <- paste0("Signals_sent_", hr1, "_", hr2)

  }

  input_string <- paste0(signals, "_", hr1, "_", hr2) # Should be length > 1

  # Sum columns and only return `Signals_sent_` prefixed column
  data %>%
    dplyr::transmute(!!sym(full_string) := select(., input_string) %>%
                       apply(1, sum, na.rm = TRUE))
}
