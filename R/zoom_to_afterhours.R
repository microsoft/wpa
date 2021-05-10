# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Take a clean Zoom participants file and compute after-hours metrics
#'
#' @description
#' Internal function used within `zoom_to_pq()`, where a formatted Zoom
#' participants file is used to compute after-hours metrics.
#'
#'
#' @param data data frame containing a clean Zoom participants file.
#' @param wowa_file data frame containing a Ways of Working Assessment flexible
#' query.
#' @param utc_offset string with the format 'hh:mm' to take into account the
#' time difference between the Zoom Admin's timezone and UTC.
#'
#' @export

zoom_to_afterhours <- function(data,
                               wowa_file,
                               utc_offset = NULL){

  if(is.null(utc_offset)){

    stop(
      paste(
        "Please provide a value to `utc_offset`.",
        "This should be in the format of 'HH:MM'."
      )
    )
  }

  # Load in timezone lookup file
  tz_df <- readRDS(file = "internal/UTC_offset.rds")

  # Assume that only Join and Leave Times matter
  lt_vars <- c(
    "Join_Time",
    "Leave_Time"
  )

  wowa_tz <-
    wowa_df %>%
    mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
    select(
      PersonId,
      HashID,
      Date,
      TimeZone,
      WorkingStartTimeSetInOutlook,
      WorkingEndTimeSetInOutlook
    ) %>%
    left_join(
      tz_df %>%
        select(WpA_TimeZone,
               UTC_offset),
      by = c("TimeZone" = "WpA_TimeZone")
    )

  # Extract times only -----------------------------------------------------
  datetime_to_numeric <- function(x){
    x %>%
      format(format="%H%M") %>%
      as.numeric()
  }

  datetime_to_hm <- function(x){
    split_hm(datetime_to_numeric(x))
  }

  remove_colon <- function(x){
    gsub(pattern = ":", replacement = "", x = x)
  }

  # Difference between Zoom Admin TimeZone and UTC -------------------------

  diff_tz <- datetime_to_numeric(remove_colon(utc_offset))
  diff_tz_hm <- split_hm(remove_colon(utc_offset))

  # Interim output 1 -------------------------------------------------------

  interim1 <-
    data %>%
    mutate(Date = lubridate::floor_date(Start_Time, unit = "weeks",
                                        week_start = 7) %>%
             as.Date()) %>% # Start on Sunday
    select(User_Email_2, Date, Join_Time, Leave_Time, UniqueMeetingID) %>%
    filter(!is.na(User_Email_2)) %>%
    left_join(
      wowa_tz,
      by = c(
        "User_Email_2" = "HashID",
        "Date"
      )
    ) %>%
    mutate(WorkingStartTimeSetInOutlook = split_hm(remove_colon(WorkingStartTimeSetInOutlook)),
           WorkingEndTimeSetInOutlook = split_hm(remove_colon(WorkingEndTimeSetInOutlook)),
           Join_Time_Admin = datetime_to_hm(Join_Time), # Zoom admin's timezone
           Leave_Time_Admin = datetime_to_hm(Leave_Time), # Zoom admin's timezone
           Join_Time_UTC = hms::as_hms(Join_Time_Admin - diff_tz_hm),
           Leave_Time_UTC = hms::as_hms(Leave_Time_Admin - diff_tz_hm),
           UTC_offset = split_hm(UTC_offset),
           Join_Time_Local = hms::as_hms(Join_Time_UTC + UTC_offset),
           Leave_Time_Local = hms::as_hms(Leave_Time_UTC + UTC_offset)
    ) %>%
    # Start earlier than settings
    mutate(Start_AH = WorkingStartTimeSetInOutlook - Join_Time_Local) %>%
    # End later than settings
    mutate(End_AH = Leave_Time_Local - WorkingEndTimeSetInOutlook)


  units(interim1$Start_AH) <- "hours"
  units(interim1$End_AH) <- "hours"

  interim1 %>%
    mutate(Start_AH = ifelse(Start_AH > 0, Start_AH, as.difftime(0, units = "hours"))) %>%
    mutate(End_AH = ifelse(End_AH > 0, End_AH, as.difftime(0, units = "hours"))) %>%
    mutate(After_hours = Start_AH + End_AH) %>%
    mutate(IsAfterhours = After_hours > 0)
}




