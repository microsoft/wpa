# --------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See LICENSE.txt in the project root for license information.
# --------------------------------------------------------------------------------------------

#' @title Collaboration Summary
#'
#' @description
#' Provides an overview analysis of 'Weekly Digital Collaboration'.
#' Returns a stacked bar plot of Email and Meeting Hours by default.
#' Additional options available to return a summary table.
#'
#' @details
#' Uses the metrics `Meeting_hours`, `Email_hours`, `Unscheduled_Call_hours`,
#' and `Instant_Message_hours`.
#'
#' @template spq-params
#' @param return Character vector specifying what to return, defaults to "plot".
#' Valid inputs are "plot" and "table".
#'
#' @import dplyr
#' @import ggplot2
#' @import reshape2
#' @import scales
#' @importFrom stats reorder
#'
#' @family Visualization
#' @family Collaboration
#'
#' @return
#' Returns a 'ggplot' object by default, where 'plot' is passed in `return`.
#' When 'table' is passed, a summary table is returned as a data frame.
#'
#' @export

collaboration_sum <- function(data,
                              hrvar = "Organization",
                              mingroup=5,
                              return = "plot"){

  if("Instant_message_hours" %in% names(data)){

    data <- rename(data, Instant_Message_hours = "Instant_message_hours")

  }

  if("Unscheduled_Call_hours" %in% names(data)){

    main_vars <- c("Meeting_hours",
                   "Email_hours",
                   "Instant_Message_hours",
                   "Unscheduled_Call_hours")

  } else {

    main_vars <- c("Meeting_hours",
                   "Email_hours")

  }

 create_stacked(data = data,
                hrvar = hrvar,
                metrics = main_vars,
                mingroup = mingroup,
                return = return)

}

#' @rdname collaboration_sum
#' @export
collab_sum <- collaboration_sum

#' @rdname collaboration_sum
#' @export
collaboration_summary <- collaboration_sum

#' @rdname collaboration_sum
#' @export
collab_summary <- collaboration_sum




