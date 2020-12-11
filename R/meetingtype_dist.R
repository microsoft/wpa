#' @title Distribution of Meeting Types by number of Attendees and Duration
#'
#' @description
#' Calculate the hour distribution of internal meeting types.
#' This is a wrapper around `meetingtype_dist_mt()` and
#' `meetingtype_dist_ca()`, depending on whether a Meeting Query or a Ways of Working Assessment Query
#' is passed as an input.
#'
#' @param data Data frame. If a meeting query, must contain the variables `Attendee` and `DurationHours`.
#' @param hrvar Character string to specify the HR attribute to split the data by.
#' Note that this is only applicable if a Ways of Working Assessment query is passed to the function. If a Meeting Query
#' is passed instead, this argument is ignored.
#' @param mingroup Numeric value setting the privacy threshold / minimum group size. Defaults to 5.
#' Only applicable when using a Ways of Working Assessment query.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats setNames
#'
#' @examples
#' ## Implementation using Standard Meeting Query
#' meetingtype_dist(mt_data)
#'
#' @export

meetingtype_dist <- function(data,
                             hrvar = NULL,
                             mingroup = 5,
                             return = "plot"){

  if("MeetingId" %in% names(data)){

    message("Calculating results using a Meeting Query...")
    meetingtype_dist_mt(data, return = return)

  } else if("PersonId" %in% names(data)){

    message("Calculating results using a Ways of Working Assessment Query...")
    meetingtype_dist_ca(data, hrvar = hrvar, mingroup = mingroup, return = return)

  } else {
    stop("Please check query type. Must be either a Ways of Working Assessment Query or a Meeting Query.")
  }
}
