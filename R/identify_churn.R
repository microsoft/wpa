# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Identify employees who have churned from the dataset
#'
#' @description
#' This function identifies and counts the number of employees who have churned
#' from the dataset by measuring whether an employee who is present in the first
#' `n` (n1) weeks of the data is present in the last `n` (n2) weeks of the data.
#'
#' @details
#' An additional use case of this function is the ability to identify
#' "new-joiners" by using the argument `flip`.
#'
#' @param data A Person Query as a data frame. Must contain a `PersonId`.
#' @param n1 A numeric value specifying the number of weeks at the beginning of
#'   the period that defines the measured employee set. Defaults to 6.
#' @param n2 A numeric value specifying the number of weeks at the end of the
#'   period to calculate whether employees have churned from the data. Defaults
#'   to 6.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"message"` (default)
#'   - `"text"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @param flip Logical, defaults to FALSE. This determines whether to reverse
#'   the logic of identifying the non-overlapping set. If set to `TRUE`, this
#'   effectively identifies new-joiners, or those who were not present in the
#'   first n weeks of the data but were present in the final n weeks.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"message"`: Message on console. A diagnostic message.
#'   - `"text"`: String. A diagnostic message.
#'   - `"data"`: Character vector containing the the `PersonId` of
#'   employees who have been identified as churned.
#'
#' @details
#' If an employee is present in the first `n` weeks of the data but not present
#' in the last `n` weeks of the data, the function considers the employee as
#' churned. As the measurement period is defined by the number of weeks from the
#' start and the end of the passed data frame, you may consider filtering the
#' dates accordingly before running this function.
#'
#' Another assumption that is in place is that any employee whose `PersonId` is
#' not available in the data has churned. Note that there may be other reasons
#' why an employee's `PersonId` may not be present, e.g. maternity/paternity
#' leave, Workplace Analytics license has been removed, shift to a
#' low-collaboration role (to the extent that he/she becomes inactive).
#'
#' @family Data Validation
#'
#' @examples
#' sq_data %>% identify_churn(n1 = 3, n2 = 3, return = "message")
#'
#' @export
identify_churn <- function(data,
                           n1 = 6,
                           n2 = 6,
                           return = "message",
                           flip = FALSE){

  data$Date <- as.Date(data$Date, format = "%m/%d/%Y") # Ensure correct format

  unique_dates <- unique(data$Date) # Vector of unique dates

  nlen <- length(unique_dates) # Total number of unique dates

  # First and last n weeks
  firstnweeks <- sort(unique_dates)[1:n1]
  lastnweeks <- sort(unique_dates, decreasing = TRUE)[1:n2]

  ## People in the first week
  first_peeps <-
    data %>%
    dplyr::filter(Date %in% firstnweeks) %>%
    dplyr::pull(PersonId) %>%
    unique()

  ## People in the last week
  final_peeps <-
    data %>%
    dplyr::filter(Date %in% lastnweeks) %>%
    dplyr::pull(PersonId) %>%
    unique()

  if(flip == FALSE){

    ## In first, not in last
    churner_id <- setdiff(first_peeps, final_peeps)

    ## Message
    printMessage <-
      paste0("Churn:\nThere are ", length(churner_id),
             " employees from ", min(firstnweeks), " to ",
             max(firstnweeks), " (", n1, " weeks)",
             " who are no longer present in ",
             min(lastnweeks), " to ", max(lastnweeks),
             " (", n2, " weeks).")

  } else if(flip == TRUE){

    ## In last, not in first
    ## new joiners
    churner_id <- dplyr::setdiff(final_peeps, first_peeps)

    ## Message
    printMessage <-
      paste0("New joiners:\nThere are ", length(churner_id),
             " employees from ", min(lastnweeks), " to ",
             max(lastnweeks), " (", n2, " weeks)",
             " who were not present in ",
             min(firstnweeks), " to ", max(firstnweeks),
             " (", n1, " weeks).")

  } else {

    stop("Invalid argument for `flip`")

  }



  if(return == "message"){

    message(printMessage)

  } else if(return == "text"){

    printMessage

  } else if(return == "data"){

    churner_id

  } else {

    stop("Invalid `return`")
  }
}
