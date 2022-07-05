# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify date frequency based on a series of dates
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Takes a vector of dates and identify whether the frequency is 'daily',
#' 'weekly', or 'monthly'. The primary use case for this function is to provide
#' an accurate description of the query type used and for raising errors should
#' a wrong date grouping be used in the data input.
#'
#' @param x Vector containing a series of dates.
#'
#' @details
#' Date frequency detection works as follows:
#'   - If at least three days of the week are present (e.g., Monday, Wednesday,
#'   Thursday) in the series, then the series is classified as 'daily'
#'   - If the total number of months in the series is equal to the length, then
#'   the series is classified as 'monthly'
#'   - If the total number of sundays in the series is equal to the length of
#'   the series, then the series is classified as 'weekly
#'
#' @section Limitations:
#' One of the assumptions made behind the classification is that weeks are
#' denoted with Sundays, hence the count of sundays to measure the number of
#' weeks. In this case, weeks where a Sunday is missing would result in an
#' 'unable to classify' error.
#'
#' Another assumption made is that dates are evenly distributed, i.e. that the
#' gap between dates are equal. If dates are unevenly distributed, e.g. only two
#' days of the week are available for a given week, then the algorithm will fail
#' to identify the frequency as 'daily'.
#'
#' @return
#' String describing the detected date frequency, i.e.:
#'   - 'daily'
#'   - 'weekly'
#'   - 'monthly'
#'
#' @examples
#' start_date <- as.Date("2022/06/26")
#' end_date <- as.Date("2022/11/27")
#'
#' # Daily
#' day_seq <-
#'   seq.Date(
#'     from = start_date,
#'     to = end_date,
#'     by = "day"
#'   )
#'
#' identify_datefreq(day_seq)
#'
#' # Weekly
#' week_seq <-
#'   seq.Date(
#'     from = start_date,
#'     to = end_date,
#'     by = "week"
#'   )
#'
#' identify_datefreq(week_seq)
#'
#' # Monthly
#' month_seq <-
#'   seq.Date(
#'     from = start_date,
#'     to = end_date,
#'     by = "month"
#'   )
#' identify_datefreq(month_seq)
#'
#' @export

identify_datefreq <- function(x){

  # Data frame for checking
  date_df <- data.frame(
    weekdays = names(table(weekdays(x))),
    n = as.numeric(table(weekdays(x)))
  )


  dweekchr <- c(
    "Sunday",
    "Saturday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday"
  )

  # At least 3 days of the week must be present
  check_wdays <- ifelse(
    sum(dweekchr %in% date_df$weekdays) >= 3, TRUE, FALSE)

  # Check number of Sundays - should equal number of weeks if weekly
  check_nsun <- sum(date_df$n[date_df$weekdays == "Sunday"])

  ifelse(
    length(months(x)) == length(x),
    "monthly",
    ifelse(
      check_nsun == length(x),
      "weekly",
      ifelse(
        check_wdays,
        "daily",
        "Unable to identify date frequency."
      )
    )
  )
}
