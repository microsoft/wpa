# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Sample Standard Person Query dataset
#'
#' @description
#' A dataset generated from a Standard Person Query from Workplace Analytics.
#'
#' @family Data
#'
#' @return data frame.
#'
#' @format A data frame with 13442 rows and 66 variables:
#' \describe{
#'   \item{PersonId}{ }
#'   \item{Date}{ }
#'   \item{Workweek_span}{Time between the person's first sent email or meeting attended and
#'    the last email or meeting for each day of the work week.}
#'   \item{Meetings_with_skip_level}{ }
#'   \item{Meeting_hours_with_skip_level}{ }
#'   \item{Generated_workload_email_hours}{ }
#'   \item{Generated_workload_email_recipients}{ }
#'   \item{Generated_workload_instant_messages_hours}{ }
#'   \item{Generated_workload_instant_messages_recipients}{ }
#'   \item{Generated_workload_call_hours}{ }
#'   \item{Generated_workload_call_participants}{ }
#'   \item{Generated_workload_calls_organized}{ }
#'   \item{External_network_size}{ }
#'   \item{Internal_network_size}{ }
#'   \item{Networking_outside_company}{ }
#'   \item{Networking_outside_organization}{ }
#'   \item{After_hours_meeting_hours}{ }
#'   \item{Open_1_hour_block}{ }
#'   \item{Open_2_hour_blocks}{ }
#'   \item{Total_focus_hours}{ }
#'   \item{Low_quality_meeting_hours}{ }
#'   \item{Total_emails_sent_during_meeting}{ }
#'   \item{Meetings}{ }
#'   \item{Meeting_hours}{ }
#'   \item{Conflicting_meeting_hours}{ }
#'   \item{Multitasking_meeting_hours}{ }
#'   \item{Redundant_meeting_hours__lower_level_}{ }
#'   \item{Redundant_meeting_hours__organizational_}{ }
#'   \item{Time_in_self_organized_meetings}{ }
#'   \item{Meeting_hours_during_working_hours}{ }
#'   \item{Generated_workload_meeting_attendees}{ }
#'   \item{Generated_workload_meeting_hours}{ }
#'   \item{Generated_workload_meetings_organized}{ }
#'   \item{Manager_coaching_hours_1_on_1}{ }
#'   \item{Meetings_with_manager}{ }
#'   \item{Meeting_hours_with_manager}{ }
#'   \item{Meetings_with_manager_1_on_1}{ }
#'   \item{Meeting_hours_with_manager_1_on_1}{ }
#'   \item{After_hours_email_hours}{ }
#'   \item{Emails_sent}{ }
#'   \item{Email_hours}{Number of hours the person spent sending and receiving emails.}
#'   \item{Working_hours_email_hours}{ }
#'   \item{After_hours_instant_messages}{ }
#'   \item{Instant_messages_sent}{ }
#'   \item{Instant_Message_hours}{ }
#'   \item{Working_hours_instant_messages}{ }
#'   \item{After_hours_collaboration_hours}{ }
#'   \item{Collaboration_hours}{ }
#'   \item{Collaboration_hours_external}{ }
#'   \item{Working_hours_collaboration_hours}{ }
#'   \item{After_hours_in_calls}{ }
#'   \item{Total_calls}{ }
#'   \item{Call_hours}{ }
#'   \item{Working_hours_in_calls}{ }
#'   \item{Domain}{ }
#'   \item{FunctionType}{ }
#'   \item{LevelDesignation}{ }
#'   \item{Layer}{ }
#'   \item{Region}{ }
#'   \item{Organization}{ }
#'   \item{zId}{ }
#'   \item{attainment}{ }
#'   \item{TimeZone}{ }
#'   \item{HourlyRate}{ }
#'   \item{IsInternal}{ }
#'   \item{IsActive}{ }
#'
#'   ...
#' }
#' @source \url{https://workplaceanalytics-demo.office.com/en-us/Home}
"sq_data"
