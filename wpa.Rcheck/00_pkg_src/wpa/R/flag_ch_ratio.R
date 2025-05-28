# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Flag unusual high collaboration hours to after-hours collaboration
#'   hours ratio
#'
#' @description This function flags persons who have an unusual ratio
#' of collaboration hours to after-hours collaboration hours.
#' Returns a character string by default.
#'
#' @template ch
#'
#' @import dplyr
#'
#' @param data A data frame containing a Person Query.
#' @param threshold Numeric value specifying the threshold for flagging.
#'   Defaults to 30.
#' @param return String to specify what to return. Options include:
#'   - `"message"`
#'   - `"text"`
#'   - `"data"`
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"message"`: message in the console containing diagnostic summary
#'   - `"text"`: string containing diagnotic summary
#'   - `"data"`: data frame. Person-level data with flags on unusually high or
#'   low ratios
#'
#' @family Data Validation
#'
#' @examples
#' flag_ch_ratio(sq_data)
#'
#'
#' data.frame(PersonId = c("Alice", "Bob"),
#'            Collaboration_hours = c(30, 0.5),
#'            After_hours_collaboration_hours = c(0.5, 30)) %>%
#'   flag_ch_ratio()
#'
#' @export
flag_ch_ratio <- function(data, threshold = c(1, 30), return = "message"){

  min_thres <- min(threshold, na.rm = TRUE)
  max_thres <- max(threshold, na.rm = TRUE)

  ## Check for high collab hours but lower afterhour collab hours
  ## Because of faulty outlook settings
  ch_summary <-
    data %>%
    group_by(PersonId) %>%
    summarise_at(vars(Collaboration_hours, After_hours_collaboration_hours), ~mean(.)) %>%
    mutate(CH_ratio = Collaboration_hours / After_hours_collaboration_hours) %>%
    arrange(desc(CH_ratio)) %>%
    mutate(CH_FlagLow = ifelse(CH_ratio < min_thres, TRUE, FALSE),
           CH_FlagHigh = ifelse(CH_ratio > max_thres, TRUE, FALSE),
           CH_Flag = ifelse(CH_ratio > max_thres | CH_ratio < min_thres, TRUE, FALSE))

  ## Percent of people with high collab hours + low afterhour collab hours
  CHFlagN <- sum(ch_summary$CH_Flag, na.rm = TRUE)
  CHFlagProp <- mean(ch_summary$CH_Flag, na.rm = TRUE)
  CHFlagProp2 <- paste(round(CHFlagProp * 100), "%") # Formatted

  CHFlagMessage_Warning <- paste0("[Warning]  The ratio of after-hours collaboration to total collaboration hours is outside the expected threshold for ", CHFlagN, " employees (", CHFlagProp2, " of the total).")
  CHFlagMessage_Pass_Low <- paste0("[Pass] The ratio of after-hours collaboration to total collaboration hours is outside the expected threshold for only ", CHFlagN, " employees (", CHFlagProp2, " of the total).")
  CHFlagMessage_Pass_Zero <- paste0("[Pass] The ratio of after-hours collaboration to total collaboration hours falls within the expected threshold for all employees.")


  CHFlagLowN <- sum(ch_summary$CH_FlagLow, na.rm = TRUE)
  CHFlagLowProp <- mean(ch_summary$CH_FlagLow, na.rm = TRUE)
  CHFlagLowProp2 <- paste(round(CHFlagLowProp * 100), "%") # Formatted
  CHFlagLowMessage <- paste0("- ", CHFlagLowN, " employees (", CHFlagLowProp2,
                          ") have an unusually low after-hours collaboration")

  CHFlagHighN <- sum(ch_summary$CH_FlagHigh, na.rm = TRUE)
  CHFlagHighProp <- mean(ch_summary$CH_FlagHigh, na.rm = TRUE)
  CHFlagHighProp2 <- paste(round(CHFlagHighProp * 100), "%") # Formatted
  CHFlagHighMessage <- paste0("- ", CHFlagHighN, " employees (", CHFlagHighProp2 , ") have an unusually high after-hours collaboration (relative to weekly collaboration hours)")

  if(CHFlagProp >= .05){
    CHFlagMessage <- paste(CHFlagMessage_Warning, CHFlagHighMessage, CHFlagLowMessage, sep = "\n")
  } else if(CHFlagProp < .05 & CHFlagProp2 > 0){
    CHFlagMessage <- paste(CHFlagMessage_Pass_Low, CHFlagHighMessage, CHFlagLowMessage, sep = "\n")
  } else if(CHFlagProp==0){
    CHFlagMessage <- CHFlagMessage_Pass_Zero
  }

  ## Print diagnosis
  ## Should implement options to return the PersonIds or a full data frame
  if(return == "message"){

    message(CHFlagMessage)

  } else if(return == "text"){

    CHFlagMessage

  } else if(return == "data") {

    ch_summary

  } else {

    stop("Invalid input for `return`.")

  }
}
