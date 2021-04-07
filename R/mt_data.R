# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Sample Meeting Query dataset
#'
#' @description
#' A dataset generated from a Meeting Query from Workplace Analytics.
#'
#' @family Data
#'
#' @return data frame.
#'
#' @format A data frame with 2001 rows and 30 variables:
#' \describe{
#'   \item{MeetingId}{ }
#'   \item{StartDate}{ }
#'   \item{StartTimeUTC}{ }
#'   \item{EndDate}{ }
#'   \item{EndTimeUTC}{ }
#'   \item{Attendee_meeting_hours}{ }
#'   \item{Attendees}{ }
#'   \item{Organizer_Domain}{ }
#'   \item{Organizer_FunctionType}{ }
#'   \item{Organizer_LevelDesignation}{ }
#'   \item{Organizer_Layer}{ }
#'   \item{Organizer_Region}{ }
#'   \item{Organizer_Organization}{ }
#'   \item{Organizer_zId}{ }
#'   \item{Organizer_attainment}{ }
#'   \item{Organizer_TimeZone}{ }
#'   \item{Organizer_HourlyRate}{ }
#'   \item{Organizer_IsInternal}{ }
#'   \item{Organizer_PersonId}{ }
#'   \item{IsCancelled}{ }
#'   \item{DurationHours}{ }
#'   \item{IsRecurring}{ }
#'   \item{Subject}{ }
#'   \item{TotalAccept}{ }
#'   \item{TotalNoResponse}{ }
#'   \item{TotalDecline}{ }
#'   \item{TotalNoEmailsDuringMeeting}{ }
#'   \item{TotalNoDoubleBooked}{ }
#'   \item{TotalNoAttendees}{ }
#'   \item{MeetingResources}{ }
#'   \item{Attendees_with_conflicting_meetings}{ }
#'   \item{Invitees}{ }
#'   \item{Emails_sent_during_meetings}{ }
#'   \item{Attendees_multitasking}{ }
#'   \item{Redundant_attendees}{ }
#'   \item{Total_meeting_cost}{ }
#'   \item{Total_redundant_hours}{ }
#'
#'   ...
#' }
#' @source \url{https://workplaceanalytics-demo.office.com/en-us/Home}
"mt_data"
