#' @title Create a summary bar chart of the proportion of Meeting Hours spent in
#'   Long or Large Meetings
#'
#' @description
#' This function creates a bar chart showing the percentage of meeting hours
#' which are spent in long or large meetings.
#'
#' @param data Ways of Working Assessment query in the form of a data frame.
#'   Requires the following variables:
#' - `Bloated_meeting_hours`
#' - `Lengthy_meeting_hours`
#' - `Workshop_meeting_hours`
#' - `All_hands_meeting_hours`
#' - `Status_update_meeting_hours`
#' - `Decision_making_meeting_hours`
#' - `One_on_one_meeting_hours`
#'
#' @param hrvar HR Variable by which to split metrics, defaults to "Organization"
#' but accepts any character vector, e.g. "LevelDesignation"
#' @param mingroup Numeric value setting the privacy threshold / minimum group
#'   size. Defaults to 5.
#' @param return String specifying what to return. This must be one of the
#'   following strings:
#'   - `"plot"`
#'   - `"table"`
#'
#' See `Value` for more information.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @family Visualization
#' @family Meetings
#'
#' @export
meetingtype_summary <- function(data,
                                hrvar = "Organization",
                                mingroup = 5,
                                return = "plot"){

  ## Handling NULL values passed to hrvar
  if(is.null(hrvar)){
    data <- totals_col(data)
    hrvar <- "Total"
  }

  mt_dist_str <- c("Bloated_meeting_hours",
                   "Lengthy_meeting_hours",
                   "Workshop_meeting_hours",
                   "All_hands_meeting_hours",
                   "Status_update_meeting_hours",
                   "Decision_making_meeting_hours",
                   "One_on_one_meeting_hours")

  returnTable <-
    data %>%
    group_by(!!sym(hrvar)) %>%
    summarise_at(vars(mt_dist_str), ~sum(., na.rm = TRUE)) %>%
    gather(MeetingType, AttendeeMeetingHours, -!!sym(hrvar)) %>%
    mutate(MeetingType = gsub(pattern = "_meeting_hours", replacement = "", x = MeetingType)) %>%
    mutate(MeetingType = us_to_space(MeetingType)) %>%
    group_by(!!sym(hrvar)) %>%
    mutate(AttendeeMeetingHours = AttendeeMeetingHours / sum(AttendeeMeetingHours)) %>%
    spread(MeetingType, AttendeeMeetingHours) %>%
    left_join(hrvar_count(data, hrvar, return = "table"), by = hrvar) %>%
    filter(n >= mingroup) %>%
    ungroup() %>%
    mutate(MeetingHoursInLongOrLargeMeetings = select(., c("All hands", "Bloated", "Lengthy", "Workshop")) %>%
             apply(1, sum, na.rm = TRUE)) %>%
    select(!!sym(hrvar), MeetingHoursInLongOrLargeMeetings, n)

  if(return == "plot"){

    returnTable %>%
      create_bar_asis(group_var = hrvar,
                      bar_var = "MeetingHoursInLongOrLargeMeetings",
                      title = "% of Meeting Hours\nin Long or Large Meetings",
                      subtitle = paste0("By ", camel_clean(hrvar)),
                      caption = extract_date_range(data, return = "text"),
                      percent = TRUE,
                      bar_colour = "alert")

  } else if(return == "table"){

    returnTable

  } else {

    stop("Please enter a valid input for `return`.")

  }
}

#' @rdname meetingtype_summary
#' @export
meetingtype_sum <- meetingtype_summary

