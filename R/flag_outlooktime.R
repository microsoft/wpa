# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Flag unusual outlook time settings for work day start and end time
#'
#' @description This function flags unusual outlook calendar settings for
#' start and end time of work day.
#'
#' @import dplyr
#'
#' @param data A data frame containing a Person Query.
#' @param threshold A numeric vector of length two, specifying the hour threshold for flagging.
#' Defaults to c(4, 15).
#' @param return String to specify what to return.
#' Valid options include "text" (default), "message", and "data".
#'
#' @family Data Validation
#'
#' @examples
#' flag_outlooktime(dv_data)
#' @export
flag_outlooktime <- function(data, threshold = c(4, 15), return = "message"){

  # pad_times <- function(x){
  #   if(nchar(x) == 1){
  #     x <- paste0("0", x, "00")
  #   } else if(nchar(x) == 2){
  #     x <- paste0(x, "00")
  #   } else if(nchar(x) == 3){
  #     x <- paste0("0", x)
  #   } else {
  #     x
  #   }
  # }
  #
  # pad_times <- Vectorize(pad_times)

  if(any(nchar(data$WorkingStartTimeSetInOutlook) != 5 | nchar(data$WorkingEndTimeSetInOutlook) != 5)){
    stop("Please check data format for `WorkingStartTimeSetInOutlook` or `WorkingEndTimeSetInOutlook.\n
         These variables must be character vectors, and have the format `%H:%M`, such as `07:30` or `23:00`.")
  }

  clean_times <- function(x){
    out <- gsub(pattern = ":", replacement = "", x = x)
    # out <- pad_times(out)
    strptime(out, format = "%H%M")
  }

  flagged_data <-
    data %>%
    # mutate_at(vars(WorkingStartTimeSetInOutlook, WorkingEndTimeSetInOutlook), ~clean_times(.)) %>%
    mutate_at(vars(WorkingStartTimeSetInOutlook, WorkingEndTimeSetInOutlook), ~gsub(pattern = ":", replacement = "", x = .)) %>%
    mutate_at(vars(WorkingStartTimeSetInOutlook, WorkingEndTimeSetInOutlook), ~strptime(., format = "%H%M")) %>%
    mutate(WorkdayRange = as.numeric(WorkingEndTimeSetInOutlook - WorkingStartTimeSetInOutlook, units = "hours"),
           WorkdayFlag1 = WorkdayRange < threshold[[1]],
           WorkdayFlag2 = WorkdayRange > threshold[[2]],
           WorkdayFlag = WorkdayRange < threshold[[1]] | WorkdayRange > threshold[[2]]) %>%
    select(PersonId, WorkdayRange, WorkdayFlag, WorkdayFlag1, WorkdayFlag2)

  ## Short working hour settings
  FlagN1 <- sum(flagged_data$WorkdayFlag1, na.rm = TRUE)
  FlagProp1 <- mean(flagged_data$WorkdayFlag1, na.rm = TRUE)
  FlagProp1F <- paste0(round(FlagProp1 * 100, 1), "%") # Formatted

  ## Long working hour settings
  FlagN2 <- sum(flagged_data$WorkdayFlag2, na.rm = TRUE)
  FlagProp2 <- mean(flagged_data$WorkdayFlag2, na.rm = TRUE)
  FlagProp2F <- paste0(round(FlagProp2 * 100, 1), "%") # Formatted

  ## Short or long working hoursettings
  FlagN <- sum(flagged_data$WorkdayFlag, na.rm = TRUE)
  FlagProp <- mean(flagged_data$WorkdayFlag, na.rm = TRUE)
  FlagPropF <- paste0(round(FlagProp * 100, 1), "%") # Formatted

  ## Flag Messages
  Warning_Message <- paste0("[Warning]  ", FlagPropF, " (", FlagN, ") ", "of the person-date rows in the data have extreme Outlook settings.")
  Pass_Message1 <- paste0("[Pass] Only ", FlagPropF, " (", FlagN, ") ", "of the person-date rows in the data have extreme Outlook settings.")
  Pass_Message2 <- paste0("There are no extreme Outlook settings in this dataset (Working hours shorter than  ", threshold[[1]], " hours, or longer than ", threshold[[2]], " hours.")
  Detail_Message <-  paste0(FlagProp1F, " (", FlagN1, ") ", " have an Outlook workday shorter than ", threshold[[1]], " hours, while ",
                        FlagProp2F, " (", FlagN2, ") ", "have a workday longer than ", threshold[[2]], " hours.")


  if(FlagProp >= .05){
    FlagMessage <- paste(Warning_Message, Detail_Message, sep = "\n")

  } else if(FlagProp < .05 & FlagProp > 0){
    FlagMessage <- paste(Pass_Message1, Detail_Message, sep = "\n")

  } else if(FlagProp==0){
    FlagMessage <- Pass_Message2
  }

  ## Print diagnosis
  ## Should implement options to return the PersonIds or a full data frame
  if(return == "text"){
    FlagMessage
  } else if(return == "message"){
    message(FlagMessage)
  } else if(return == "data"){
    flagged_data[flagged_data$WorkdayFlag == TRUE,]
  } else {
    stop("Error: please check inputs for `return`")
  }
}


