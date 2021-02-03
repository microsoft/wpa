#' @title Classify working pattern personas using a rule based algorithm
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' Apply a rule based algorithm to emails or instant messages sent by hour of day.
#' Uses a binary week-based (bw) method by default, with options to use the
#' the person-average volume-based (pav) method.
#'
#' @details
#' This is a wrapper around `workpatterns_classify_bw()` and `workpatterns_classify_pav()`.
#'
#' @param data A data frame containing data from the Hourly Collaboration query.
#'
#' @param hrvar A string specifying the HR attribute to cut the data by.
#' Defaults to NULL. This only affects the function when "table" is returned,
#' and is only applicable for method = "bw".
#'
#' @param return Character vector to specify what to return. Valid options include:
#'   - "plot": returns a heatmap plot of signal distribution by hour and archetypes (default)
#'   - "data": returns the raw data with the classified archetypes
#'   - "table": returns a summary table of the archetypes
#'   - "plot-area": returns an area plot of the percentages of archetypes shown over time
#'
#' @param method String to pass through specifying which method to use for classification.
#' By default, a binary week-based (bw) method is used, with options to use the
#' the person-average volume-based (pav) method.
#'
#' @param values Only valid if using `pav` method.
#' Character vector to specify whether to return percentages
#' or absolute values in "data" and "plot". Valid values are "percent" (default)
#' and "abs".
#'
#' @param signals Character vector to specify which collaboration metrics to use:
#'   - "email" (default) for emails only
#'   - "IM" for Teams messages only,
#'   - "unscheduled_calls" for Unscheduled Calls only
#'   - "meetings" for Meetings only
#'   - or a combination of signals, such as `c("email", "IM")`
#'
#' @param start_hour A character vector specifying starting hours,
#' e.g. "0900". Note that this currently only supports **hourly** increments.
#' @param end_hour A character vector specifying starting hours,
#' e.g. "1700". Note that this currently only supports **hourly** increments.
#'
#' @import dplyr
#' @import tidyselect
#' @import ggplot2
#'
#' @examples
#'
#' # Returns a plot by default
#' em_data %>% workpatterns_classify(method = "bw")
#'
#' # Return an area plot
#' em_data %>% workpatterns_classify(method = "bw", return = "plot-area")
#'
#' \dontrun{
#'
#' em_data %>% workpatterns_classify(method = "bw", return = "table")
#'
#' em_data %>% workpatterns_classify(method = "pav")
#'
#' em_data %>% workpatterns_classify(method = "pav", return = "plot-area")
#'
#' }
#'
#' @family Work Patterns
#'
#' @export
workpatterns_classify <- function(data,
                                  hrvar = "Organization",
                                  values = "percent",
                                  signals = "email",
                                  start_hour = "0900",
                                  end_hour = "1700",
                                  method = "bw",
                                  return = "plot"){

  if(method == "bw"){
    workpatterns_classify_bw(data = data,
                             hrvar = hrvar,
                             signals = signals,
                             start_hour = start_hour,
                             end_hour = end_hour,
                             return = return)
  } else if(method == "pav"){

    workpatterns_classify_pav(data = data,
                              values = values,
                              signals = signals,
                              start_hour = start_hour,
                              end_hour = end_hour,
                              return = return)
  } else {
    stop("Invalid method: please check input for `method`")
  }
}





