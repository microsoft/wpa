#' @title Meeting Hours Time Trend
#'
#' @description
#' Provides a week by week view of meeting time. By default returns a week by
#' week heatmap, highlighting the points in time with most activity. Additional
#' options available to return a summary table.
#'
#' @details
#' Uses the metric `Meeting_hours`.
#'
#' @inheritParams create_trend
#' @inherit create_trend return
#'
#' @family Visualization
#' @family Meetings
#'
#' @export

meeting_trend <- function(data,
                        hrvar = "Organization",
                        mingroup = 5,
                        return = "plot"){

  create_trend(data,
               metric = "Meeting_hours",
               hrvar = hrvar,
               mingroup = mingroup,
               return = return)

}
