#' @title Meeting Type Distribution
#'
#' @description
#' Calculate the hour distribution of internal meeting types.
#' This is a wrapper around `meetingtype_dist_mt()` and
#' `meetingtype_dist_ca()`, depending on whether a Meeting Query or a Collaboration Assessment Query
#' is passed as an input.
#'
#' @param data Data frame. If a meeting query, must contain the variables `Attendee` and `DurationHours`.
#' @param hrvar Character string to specify the HR attribute to split the data by.
#' Note that this is only applicable if a Collaboration Assessment query is passed to the function. If a Meeting Query
#' is passed instead, this argument is ignored.
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats setNames
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

    message("Calculating results using a Collaboration Assessment Query...")
    meetingtype_dist_ca(data, hrvar = hrvar, mingroup = mingroup, return = return)

  } else {
    stop("Please check query type. Must be either a Collaboration Assessment Query or a Meeting Query.")
  }
}
