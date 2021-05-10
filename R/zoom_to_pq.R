# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title
#' Convert a hashed Zoom participants data frame to a Person Query structure
#'
#' @description
#' Convert a hashed Zoom participants level data frame to one with a Person
#' Query structure, where each row represents a person-week. A Standard Meeting
#' Query as well as a Person Query is required as inputs in order to perform
#' scheduled meeting and licensed population matching.
#'
#' @param data data frame containing a hashed Zoom meeting participants dataset.
#' @param mq_key character vector containing the unique meeting subject lines
#' from the Standard Meeting Query. This is used for identifying whether calls
#' in Zoom are scheduled or unscheduled. If left blank, no matching is performed
#' and calls in the Zoom file are assumed to be unscheduled if the Meeting ID
#' and Creation Time Rules are satisfied.
#' @param wowa_file data frame containing a Ways of Working Assessment query.
#' Note that the `TimeZone` variable must be present in order for after-hours
#' metrics to be calculated correctly.
#' @param utc_offset string, with the format 'hh:mm' to represent the difference
#' of the Zoom Admin's timezone setting with respect to UTC. For example, you
#' should pass "08:00" to represent the Zoom Admin's timezone being 8 hours
#' ahead of UTC. For offsets which are _behind_ UTC, you may represent by
#' prefixing it with a minus sign, e.g. "-08:00", or directly deduct the value
#' from 24, e.g. "16:00".
#' @param return string determining what output to return. Must be one of the
#' following values:
#'   - `"minimal"`
#'   - `"full"`
#'   - `"afterhours"`
#'   - `"participants"`

zoom_to_pq <- function(data,
                       mq_key = "",
                       wowa_file = NULL,
                       utc_offset,
                       return = "minimal"){

  start_t <- Sys.time()

  # Input checks -----------------------------------------------------------

  if(is.null(wowa_file)){
    stop("Please provide a valid Ways of Working Assessment to `wowa_file`.")
  }

  # Clean data --------------------------------------------------------------

  # # Clean hash file
  # clean_hash_file <-
  #   hash_file %>%
  #   mutate(PersonID = tolower(PersonID)) # Coerce lowercase

  lt_vars <- c(
    "Creation_Time",
    "Start_Time",
    # "End_Time", # Save time
    "Join_Time",
    "Leave_Time"
  )

  date_format <- "%Y/%m/%d %H:%M:%S"

  message("Parsing dates...",
          "(", round(difftime(Sys.time(), start_t, units = "secs"), 1), " secs)")

  # Replace hash ID
  #TODO: type assert for inputs
  clean_zoom <- data %>%
    mutate(
      across(
        .cols = all_of(lt_vars),
        # .fns = ~lubridate::parse_date_time(x = ., orders = date_format)
        .fns = ~as.POSIXct(x = ., format = date_format, tz = "UTC")
      )
    )

  # Meeting Participants ----------------------------------------------------

  message("Cleaning meeting participants data...",
          "(", round(difftime(Sys.time(), start_t, units = "secs"), 1), " secs)")

  clean_participants <-
    clean_zoom %>%
    mutate(TopicLower = tolower(Topic)) %>% # Coerce to lowercase
    mutate(InSMQ = ifelse(Topic %in% tolower(mq_key), TRUE, FALSE)) %>%
    mutate(BookGap = Start_Time - Creation_Time) %>%
    mutate(BookGap = as.numeric(BookGap, "mins")) %>%
    mutate(Meeting_ID_num =
             Meeting_ID %>%
             gsub(pattern = " ", replacement = "", x = .)) %>%
    mutate(Rule1 = ifelse(InSMQ == FALSE & nchar(Meeting_ID_num) %in% c(9, 10), TRUE, FALSE)) %>%
    mutate(Rule2 = ifelse(InSMQ == FALSE & BookGap < 2 & nchar(Meeting_ID_num) == 11, TRUE, FALSE)) %>%
    mutate(IsUnscheduled = select(., Rule1, Rule2) %>% apply(1, any)) %>%
    # Convert to Hours
    mutate(DurationHours_1 = Duration_Minutes_1 / 60,
           DurationHours_2 = Duration_Minutes_2 / 60) %>%
    # Use Meeting Start Date
    mutate(Date = lubridate::floor_date(Start_Time, unit = "weeks",
                                        week_start = 7) %>%
             as.Date()) %>% # Start on Sunday
    mutate(DayOfWeek = lubridate::wday(Date, label = TRUE)) %>%
    mutate(UniqueMeetingID = paste(
      Meeting_ID,
      Topic,
      Start_Time,
      User_Email_1,
      Participants,
      sep = "_"))

  if(return == "participants"){

    message(
      paste("Total runtime : ",
            round(difftime(Sys.time(), start_t, units = "mins"), 1),
            "minutes."))

    clean_participants

  } else if(return %in% c("afterhours")){


    # Afterhours metrics ------------------------------------------------------

    message("Calculating after-hours metrics...",
            "(", round(difftime(Sys.time(), start_t, units = "secs"), 1), " secs)")

    # Debug mode
    ahmetrics_part <- # Participant level
      zoom_to_afterhours(
        data = clean_participants,
        wowa_file = wowa_df,
        utc_offset = utc_offset
      )

  } else if(return %in% c("full", "minimal")){

    # Host metrics ------------------------------------------------------------

    # message("Calculating host metrics...",
    #         "(", round(difftime(Sys.time(), start_t, units = "secs"), 1), " secs)")
    #
    # hostmetrics_pq <-
    #   clean_participants %>%
    #   filter(!is.na(User_Email_1)) %>% # Exclude unmatched hosts
    #   group_by(
    #     Date,
    #     User_Email_1 # Host ID
    #   ) %>%
    #   summarise(
    #     Generated_workload_unscheduled_call_hours =
    #       sum(DurationHours_2[IsUnscheduled == TRUE], na.rm = TRUE),
    #     .groups = "drop"
    #   )

    # Afterhours metrics ------------------------------------------------------

    message("Calculating after-hours metrics...",
            "(", round(difftime(Sys.time(), start_t, units = "secs"), 1), " secs)")

    ahmetrics_part <- # Participant level
      zoom_to_afterhours(
        data = clean_participants,
        wowa_file = wowa_df,
        utc_offset = utc_offset
      ) %>%
      select(User_Email_2,
             UniqueMeetingID,
             After_hours,
             IsAfterhours,
             Join_Time,
             Leave_Time)


    # Move to person level ----------------------------------------------------

    message("Computing metrics at a person-level... ",
            "(", round(difftime(Sys.time(), start_t, units = "secs"), 1), " secs)")

    clean_pq <-
      clean_participants %>%
      filter(!is.na(User_Email_2)) %>% # Exclude unmatched participants
      left_join(
        ahmetrics_part,
        by = c("User_Email_2",
               "UniqueMeetingID",
               "Join_Time",
               "Leave_Time")
      ) %>%
      group_by(Date, User_Email_2) %>%
      summarise(
        Zoom_Unscheduled_call_hours =
          sum(DurationHours_2[IsUnscheduled == TRUE], na.rm = TRUE),

        Zoom_Scheduled_call_hours =
          sum(DurationHours_2[IsUnscheduled == FALSE], na.rm = TRUE),

        Zoom_Total_call_hours = sum(DurationHours_2, na.rm = TRUE),

        # Unscheduled call hour types - duration ----------------------------------
        Zoom_Unscheduled_call_hours_30_mins_or_less =
          sum(DurationHours_2[IsUnscheduled == TRUE & Duration_Minutes_2 <= 30],
              na.rm = TRUE),

        Zoom_Unscheduled_call_hours_31_to_59_mins =
          sum(DurationHours_2[IsUnscheduled == TRUE & Duration_Minutes_2 >= 31
                              & Duration_Minutes_2 <= 59]
              , na.rm = TRUE),

        Zoom_Unscheduled_call_hours_1_hour =
          sum(DurationHours_2[IsUnscheduled == TRUE & Duration_Minutes_2 == 60],
              na.rm = TRUE),

        Zoom_Unscheduled_call_hours_1_to_2_hours =
          sum(DurationHours_2[IsUnscheduled == TRUE & Duration_Minutes_2 >= 61
                              & Duration_Minutes_2 <= 119]
              , na.rm = TRUE),

        Zoom_Unscheduled_call_hours_more_than_2_hours =
          sum(DurationHours_2[IsUnscheduled == TRUE & Duration_Minutes_2 >= 120],
              na.rm = TRUE),

        # Unscheduled call hour types - attendees --------------------------------

        Zoom_Unscheduled_call_hours_2_attendees =
          sum(DurationHours_2[IsUnscheduled == TRUE & Participants == 2],
              na.rm = TRUE),

        Zoom_Unscheduled_call_hours_3_to_8_attendees =
          sum(DurationHours_2[IsUnscheduled == TRUE & Participants >= 3 &
                                Participants <= 8],
              na.rm = TRUE),

        Zoom_Unscheduled_call_hours_9_to_18_attendees =
          sum(DurationHours_2[IsUnscheduled == TRUE & Participants >= 9 &
                                Participants <= 18],
              na.rm = TRUE),

        Zoom_Unscheduled_call_hours_19_or_more_attendees =
          sum(DurationHours_2[IsUnscheduled == TRUE & Participants >= 19],
              na.rm = TRUE),

        # Number of meetings ----------------------------------

        Zoom_Meetings = n_distinct(UniqueMeetingID),

        Zoom_Unscheduled_Meetings = n_distinct(UniqueMeetingID[IsUnscheduled == TRUE]),

        Zoom_Scheduled_Meetings = n_distinct(UniqueMeetingID[IsUnscheduled == FALSE]),

        # Invalid because `Start_Time` and `End_Time` is based on Host Join Time
        # Zoom_meeting_hours = sum(DurationHours_1, na.rm = TRUE),

        # After hours metrics ---------------------------------

        Zoom_After_hours_in_unscheduled_calls =
          sum(After_hours[IsUnscheduled == TRUE], na.rm = TRUE),

        Zoom_After_hours_in_scheduled_calls =
          sum(After_hours[IsUnscheduled == FALSE], na.rm = TRUE),

        # end of metric calculation
        .groups = "drop"
      ) # %>%
    # left_join(
    #   hostmetrics_pq,
    #   by = c("User_Email_2" = "User_Email_1",
    #          "Date")
    # )

    message(
      paste("Total runtime : ",
            round(difftime(Sys.time(), start_t, units = "mins"), 1),
            "minutes.")
    )

    # Return conditional -----------------------------------------------------

    if(return == "full"){

      wowa_file %>%
        mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
        left_join(clean_pq,
                  by = c("Date" = "Date",
                         "HashID" = "User_Email_2"))

    } else if(return == "minimal"){

      clean_pq <-
        clean_pq %>%
        select(
          Date,
          User_Email_2,
          Zoom_Unscheduled_call_hours,
          Zoom_Scheduled_call_hours,
          Zoom_Unscheduled_Meetings,
          Zoom_Scheduled_Meetings,
          Generated_workload_unscheduled_call_hours
        )

      wowa_file %>%
        mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
        left_join(clean_pq,
                  by = c("Date" = "Date",
                         "HashID" = "User_Email_2"))

    } else {

      stop("Invalid return.")

    }

  }

}
