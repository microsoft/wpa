# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Warn if a certain metric exceeds an arbitrary threshold
#'
#' @description
#' This is used as part of data validation to check if there are extreme values
#' in the dataset.
#'
#' @param data A Standard Person Query dataset in the form of a data frame.
#' @param metric A character string specifying the metric to test.
#' @param person A logical value to specify whether to calculate
#'   person-averages. Defaults to `TRUE` (person-averages calculated).
#' @param threshold Numeric value specifying the threshold for flagging.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"text"`
#'   - `"message"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"text"`: string. A diagnostic message.
#'   - `"message"`: message on console. A diagnostic message.
#'   - `"table"`: data frame. A person-level table with `PersonId` and the
#'   extreme values of the selected metric.
#'
#' @family Data Validation
#'
#' @import dplyr
#'
#' @examples
#' # The threshold values are intentionally set low to trigger messages.
#' flag_extreme(sq_data, "Email_hours", threshold = 15)
#'
#' # Return a summary table
#' flag_extreme(sq_data, "Email_hours", threshold = 15, return = "table")
#'
#' # Person-week level
#' flag_extreme(sq_data, "Email_hours", person = FALSE, threshold = 15)
#'
#'
#' @export
flag_extreme <- function(data,
                         metric,
                         person = TRUE,
                         threshold,
                         return = "message"){

  ## Data frame containing the extreme values
  if(person == TRUE){
    extreme_df <-
      data %>%
      rename(metric = !!sym(metric)) %>%
      group_by(PersonId) %>%
      summarise_at(vars(metric), ~mean(.)) %>%
      filter(metric > threshold) %>%
      rename(!!sym(metric) := "metric")
  } else if(person == FALSE){
    extreme_df <-
      data %>%
      rename(metric = !!sym(metric)) %>%
      filter(metric > threshold) %>%
      rename(!!sym(metric) := "metric")
  }


  ## Clean names for pretty printing
  metric_nm <- metric %>% us_to_space() %>% camel_clean()

  ## Define MessageLevel
  if(person == TRUE){
    MessageLevel <- " persons where their average "
  } else if(person == FALSE){
    MessageLevel <- " rows where their value of "
  }

  ## Define FlagMessage

  if(nrow(extreme_df) == 0){
    FlagMessage <-
      paste0("[Pass] There are no ",
             MessageLevel,
             metric_nm,
             " exceeds ",
             threshold, ".")
  } else {
    FlagMessage <-
      paste0("[Warning] There are ",
             nrow(extreme_df),
             MessageLevel,
             metric_nm,
             " exceeds ",
             threshold, ".")
  }

  if(return == "text"){

    FlagMessage

  } else if(return == "message"){

    message(FlagMessage)

  } else if(return == "table"){

    extreme_df

  }
}
