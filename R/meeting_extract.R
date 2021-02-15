# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Extract top low-engagement meetings from the Meeting Query
#'
#' @description
#' Pass a Standard Meeting Query and extract the top low engagement meetings.
#'
#' @param data Data frame containing a Standard Meeting Query to pass through.
#' @param recurring_only. Logical value indicating whether to only filter by
#'   recurring meetings.
#' @param top_n Numeric value for the top number of results to return in the
#' output.
#' @param fte_month Numeric value for the assumed number of employee hours per
#'   month for conversion calculations. Defaults to 180.
#' @param fte_week Numeric value for the assumed number of employee hours per
#'  week for conversion calculations. Defaults to 180.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"table"`
#'   - `"data"`
#'
#' See `Value` for more information.
#'
#' @return
#' A different output is returned depending on the value passed to the `return`
#' argument:
#'   - `"table"`: data frame. A summary table containing the top `n` low
#'   engagement meetings
#'   - `"data"`: data frame. Contains the full computed metrics related to the
#'   top `n` low engagement meetings
#'
#' @import dplyr
#'
#' @export
meeting_extract <- function(data,
                            recurring_only = TRUE,
                            top_n = 30,
                            fte_month = 180,
                            fte_week = 40,
                            return = "table"){

  if(recurring_only == TRUE){

    mq_df <- filter(mq_df, IsRecurring == TRUE)

  }

  mq_df <-
    data %>%
    mutate(Emails_sent_per_attendee_hour =
             Emails_sent_during_meetings / Attendee_meeting_hours) %>%
    mutate(AttendanceRate = Attendees / Invitees) %>%
    summarise(
      Sum_Attendee_meeting_hours = sum(Attendee_meeting_hours),
      Mean_Attendee_meeting_hours = mean(Attendee_meeting_hours),
      Sum_Attendees = sum(Attendees),
      Mean_Attendees = mean(Attendees),
      Sum_Emails_sent_during_meetings = sum(Emails_sent_during_meetings),
      Mean_Emails_sent_during_meetings = mean(Emails_sent_during_meetings),
      Sum_Emails_sent_per_attendee_hour = sum(Emails_sent_per_attendee_hour),
      Mean_Emails_sent_per_attendee_hour = mean(Emails_sent_per_attendee_hour),
      Sum_DurationHours = sum(DurationHours),
      Mean_DurationHours = mean(DurationHours),
      Sum_Total_meeting_cost = sum(Total_meeting_cost),
      Mean_Total_meeting_cost = mean(Total_meeting_cost),
      Mean_AttendanceRate = mean(AttendanceRate),
      Mean_Invitees = mean(Invitees),
      NRecurring = n()
    ) %>%
    filter(Mean_Emails_sent_per_attendee_hour >= 2) %>%
    arrange(desc(Sum_Attendee_meeting_hours)) %>%
    slice(1:top_n)

  if(return == "data"){

    mq_df

  } else if(return == "table"){

    mq_df %>%

      select(Subject,
             `Attendee hours` = "Sum_Attendee_meeting_hours",
             `Average emails per attendee hour` = "Mean_Emails_sent_per_attendee_hour",
             `Duration` = Mean_DurationHours,
             `Attendees` = Mean_Attendees,
             `Number of recurrences` = NRecurring,
             `Average Attendance Rate` = Mean_AttendanceRate,
             `Invitees` = Mean_Invitees) %>%
      mutate(`Equivalent FTE month` = `Attendee hours` / 180) %>%
      mutate(`Equivalent FTE week` = `Attendee hours` / 40)

  } else {

    stop("Invalid input to `return`.")

  }
}
