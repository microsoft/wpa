# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Flag Persons with unusually high Email Hours to Emails Sent ratio
#'
#' @description This function flags persons who have an unusual ratio
#' of email hours to emails sent. If the ratio between Email Hours and
#' Emails Sent is greater than the threshold, then observations tied to
#' a `PersonId` is flagged as unusual.
#'
#' @import dplyr
#'
#' @family Data Validation
#'
#' @param data A data frame containing a Person Query.
#' @param threshold Numeric value specifying the threshold for flagging.
#'   Defaults to 1.
#'
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"text"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"text"`: string. A diagnostic message.
#'   - `"data"`: data frame. Person-level data with those flagged with unusual
#'   ratios.
#'
#' @examples
#' flag_em_ratio(sq_data)
#'
#' @export
flag_em_ratio <- function(data, threshold = 1, return = "text"){

  ## Check for high collab hours but lower afterhour collab hours
  ## Because of faulty outlook settings
  em_summary <-
    data %>%
    group_by(PersonId) %>%
    summarise_at(vars(Email_hours, Emails_sent), ~mean(.)) %>%
    mutate(Email_ratio = Email_hours / Emails_sent) %>%
    arrange(desc(Email_ratio)) %>%
    mutate(Email_Flag = ifelse(Email_ratio > threshold, TRUE, FALSE))

  ## Percent of people with high collab hours + low afterhour collab hours
  EmailFlagN <- sum(em_summary$Email_Flag, na.rm = TRUE)
  EmailFlagProp <- mean(em_summary$Email_Flag, na.rm = TRUE)
  EmailFlagProp2 <- paste(round(EmailFlagProp * 100), "%") # Formatted
  EmailFlagMessage <- paste0(EmailFlagProp2, " (", EmailFlagN, ") ",
                         "of the population have an unusually high email hours to emails sent ratio.")

  if(return == "text"){

    EmailFlagMessage

  } else if(return == "data"){

    em_summary

  } else {

    stop("Invalid input to `return`.")

  }
}


